open Pst
open Errors

include Ast_types

let (%) f g x = f (g x)


let rec pattern_of_pst (p : Pst.pst) =
  match p with
  | Symbol sym -> begin
      match int_of_string_opt sym with
      | Some i -> IntPattern i 
      | None ->
          match sym with
          | "_"     -> WildcardPattern
          | "nil"   -> NilPattern
          | "true"  -> BoolPattern true
          | "false" -> BoolPattern false
          | _ -> 
              if String.get sym 0 = '\''
              then let name = (String.sub sym 1 (String.length sym - 1)) in
                if name = "" then raise (AbstractSyntaxError "Invalid empty symbol pattern");
                SymbolPattern name
              else VarPattern sym 
      end
  | Node [] -> raise (AbstractSyntaxError "Expected pattern but got '()'")
  | Node (head :: args) ->
      match head, args with
      | Pst.Symbol "cons", [p1; p2] -> ConsPattern (pattern_of_pst p1, pattern_of_pst p2)
      | Pst.Symbol s, ps -> StructPattern (s, List.map pattern_of_pst ps)
      | _ -> raise (AbstractSyntaxError ("Expected pattern, but got " ^ Pst.string_of_pst p))


let pattern_of_string = pattern_of_pst % Pstparser.pst_of_string

let rec vars_of_pattern (p : pattern) : string list = 
    match p with 
    | VarPattern s -> [s]
    | ConsPattern (p1, p2) -> vars_of_pattern p1 @ vars_of_pattern p2
    | StructPattern (s, ps) -> s :: List.fold_left (@) [] (List.map vars_of_pattern ps)
    | _ -> [] 

let pst_error info pst = AbstractSyntaxError (info ^ Pst.string_of_pst pst)

let ensure_symbol err = function 
  | Pst.Symbol s -> s 
  | _ -> raise err

let ensure_unique err l = 
  let rec aux seen = function 
    | [] -> true
    | x :: xs -> if List.mem x seen then false else aux (x :: seen) xs 
  in if aux l [] then l else raise err
  (* if List.length (List.sort_uniq (fun _ _ -> 0) l) <> List.length l  *)
  (* then raise err; l *)

let ensure_unique_symbols err_sym err_uniq = ensure_unique err_uniq % List.map (ensure_symbol err_sym) 

(* last stage of parser: converts pst to expr *)
let rec expr_of_pst p =
  let raise_expect_args n info = 
    raise (AbstractSyntaxError (info ^ " expects " ^ string_of_int n ^ " args but got " ^ Pst.string_of_pst p))

  in let op    sym c = function (* args = match args with *)
    | [p] -> c (expr_of_pst p)
    | _   -> raise_expect_args 1 ("operator " ^ sym)
  in let binop sym c = function
    | [left; right] -> c (expr_of_pst left) (expr_of_pst right)
    | _   -> raise_expect_args 2 ("operator " ^ sym)

  in match p with
  | Symbol sym -> begin
    if String.get sym 0 = '\'' 
    then let name = (String.sub sym 1 (String.length sym - 1)) in
      if name = "" then raise (AbstractSyntaxError "Invalid empty symbol");
      Symbol name
    else match int_of_string_opt sym with
    | Some i -> Int i
    | _ -> 
        match sym with
        | "true"  -> Bool true
        | "false" -> Bool false
        | "nil"   -> Nil
        | _       -> Var sym
    end

  | Node [] -> raise (AbstractSyntaxError "Expected expression but got '()'")
  | Node (head :: args) ->
      match head, args with
      (* | Node _, _ -> raise (pst_error "Expression forms must start with a symbol but got " head) *)

      | Symbol "print", args -> op    "print" (fun x   -> Print     x) args

      | Symbol "car"  , args -> op    "car"   (fun x   -> Car       x) args (* constructors should be functions! [^1] *)
      | Symbol "cdr"  , args -> op    "cdr"   (fun x   -> Cdr       x) args
      | Symbol "cons?", args -> op    "cons?" (fun x   -> IsCons    x) args
      | Symbol "nil?" , args -> op    "nil?"  (fun x   -> IsNil     x) args
      | Symbol "+"    , args -> binop "+"     (fun x y -> Add  (x, y)) args
      | Symbol "-"    , args -> binop "-"     (fun x y -> Sub  (x, y)) args
      | Symbol "*"    , args -> binop "*"     (fun x y -> Mul  (x, y)) args
      | Symbol "="    , args -> binop "="     (fun x y -> Eq   (x, y)) args
      | Symbol "cons" , args -> binop "cons"  (fun l r -> Cons (l, r)) args

      | Symbol "if"   , [branch; thn; els] -> If (expr_of_pst branch, expr_of_pst thn, expr_of_pst els)
      | Symbol "if"   , _                  -> raise_expect_args 3 "if expression" 

      | Symbol "let"  , [Node defs; body] -> 
          let bind seen = function (* TODO: refactor to use existing ensure_unique *)
          | Node [Symbol name; pst] -> 
              if List.mem name seen 
              then raise (pst_error ("Duplicate binding '" ^ name ^ "' in let expression ") p)
              else (name :: seen, (name, expr_of_pst pst)) 
          | p -> raise (pst_error "Malformed binding in let expression " p)
          in let (_, bindings) = List.fold_left_map bind [] defs
          (* let bind = function  *)
          (*   | Node [Symbol name; p] -> (name, expr_of_pst p) *)
          (*   | p -> raise (pst_error "Malformed binding in let expression " p) *)
          (* in let bindings = List.map bind defs |> ensure_unique  *)
          (*   (AbstractSyntaxError ("Duplicate let expression binding in " ^ (String.concat ", " (List.map string_of_pst defs)))) *)
          in Let (bindings, expr_of_pst body)
      | Symbol "let"  , _ -> raise_expect_args 2 "let expression"

      | Symbol "cond", clauses ->  
          let pair_clause = function 
            | Node [pred; body] -> (expr_of_pst pred, expr_of_pst body)
            | p -> raise (pst_error "Malformed clause in cond expression " p)
          in Cond (List.map pair_clause clauses)

      | Symbol "match", head :: clauses -> 
          let pair_clause = function 
            | Node [pat; body] -> 
                let p = pattern_of_pst pat in 
                p |> vars_of_pattern 
                  |> ensure_unique (AbstractSyntaxError ("Duplicate variable pattern in match expression " ^ string_of_pattern p)) 
                  |> ignore;
                (p, expr_of_pst body)
            | p -> raise (pst_error "Malformed clause in match expression" p)
          in Match (expr_of_pst head, List.map pair_clause clauses)
      | Symbol "match", _ -> raise (pst_error "Match expression missing head expression to match on " p)
      
      | Symbol "lambda", [Node params; body] -> 
          let param_names = params |> ensure_unique_symbols 
            (pst_error "Expected lambda function parameter to be a symbol but got " p)
            (pst_error "Duplicate lambda function parameter name " p)
          in Lambda { rec_name = None; lambda_param_names = param_names; lambda_body = expr_of_pst body}

      | f, args -> Call (expr_of_pst f, List.map expr_of_pst args)

(* ^1 tagging data is a transformation equivalent to placing a value into a functorial context. *)

let expr_of_string = expr_of_pst % Pstparser.pst_of_string


let binding_of_pst p = match p with
  | Pst.Symbol _ -> TopLevelExpr (expr_of_pst p)
  | Node [] -> raise (AbstractSyntaxError "Expected binding but got '()'")
  | Node (head :: args) ->
      match head, args with
      | Symbol "define", [Symbol name; value] -> VarBinding (name, expr_of_pst value)
      | Symbol "define", [Node (Symbol name :: params); body] ->
          let param_names = params |> ensure_unique_symbols
            (pst_error "Expected function parameter to be a symbol but got " p)
            (pst_error "Duplicate function parameter name " p)
          in FunctionBinding { func_name = name; param_names = param_names; body = expr_of_pst body }
      | Symbol "define", _ -> raise (pst_error "Malformed definition " p)

      | Symbol "test", [p] -> TestBinding (expr_of_pst p)
      | Symbol "test", _ -> raise (pst_error "Malformed test " p)

      | Symbol "struct", Symbol name :: fields -> 
          let field_names = fields |> ensure_unique_symbols 
            (pst_error "Expected struct field name to be a symbol but got " p)
            (pst_error "Duplicate struct field name " p)
          in StructBinding { struct_name = name; field_names = field_names }

      | Node _, _ -> raise (pst_error "Expected binding to start with a symbol but got " p)

      | _ -> TopLevelExpr (expr_of_pst p)


let binding_of_string = binding_of_pst % Pstparser.pst_of_string

let bindings_of_string s =
  let p = Pstparser.pstparser_of_string s in
  let rec parse_binding_list () =
    match Pstparser.parse_pst p with
    | None -> []
    | Some pst -> binding_of_pst pst :: parse_binding_list ()
  in parse_binding_list ()
