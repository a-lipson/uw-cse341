include Ast_types
open Pst
open Errors

let rec pattern_of_pst p =
  match p with
  | Pst.Symbol sym -> begin
      match int_of_string_opt sym with
      | Some n -> failwith "TODO: build an int pattern with n here"
      | None ->
         match sym with
         | "_" -> WildcardPattern
         | "true" -> failwith "TODO: build a bool pattern with true here"
         (* TODO: add other cases here for "false" and "nil" *)
         | _ ->
            if String.get sym 0 = '\'' (* if the string starts with an apostrophe *)
            then let sym_without_apostrophe = String.sub sym 1 (String.length sym - 1)
                 in failwith "TODO: build a symbol pattern using sym_without_apostrophe"
            else failwith "TODO: build a variable pattern using sym"
    end
  | Pst.Node [] -> raise (AbstractSyntaxError "Expected pattern but got '()'")
  | Pst.Node (head :: args) ->
     match head, args with
     | Pst.Symbol "cons", [p1; p2] -> ConsPattern (pattern_of_pst p1, pattern_of_pst p2)
     | Pst.Symbol s, ps -> failwith "TODO: build a struct pattern using patterns ps"
     | _ -> raise (AbstractSyntaxError ("Expected pattern, but got " ^ Pst.string_of_pst p))

let pattern_of_string s =
  s
  |> Pstparser.pst_of_string
  |> pattern_of_pst
(* last stage of parser: converts pst to expr *)
let rec expr_of_pst p =
  let raise_expect_args n info = 
    raise (AbstractSyntaxError (info ^ " expects " ^ string_of_int n ^ " args but got " ^ Pst.string_of_pst p))
  in let raise_explain_pst info pst =
    raise (AbstractSyntaxError (info ^ Pst.string_of_pst pst))
  in let op    sym c = function (* args = match args with *)
    | [p] -> c (expr_of_pst p)
    | _   -> raise_expect_args 1 ("operator " ^ sym)
  in let binop sym c = function
    | [left; right] -> c (expr_of_pst left) (expr_of_pst right)
    | _   -> raise_expect_args 2 ("operator " ^ sym)

  in match p with
  | Symbol sym -> begin
     try Int (int_of_string sym) with
       Failure _ ->
       match sym with
       | "true"  -> Bool true
       | "false" -> Bool false
       | "nil"   -> Nil
       | _       -> Var sym
    end

  | Node [] -> raise (AbstractSyntaxError "Expected expression but got '()'")
  | Node (head :: args) ->
      match head, args with
      | Node _, _ -> raise_explain_pst "Expression forms must start with a symbol, but got " head

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
          let bind_def seen = function
          | Node [Symbol name; pst] -> 
              if List.mem name seen 
              then raise_explain_pst ("Duplicate binding: " ^ name) p
              else (name :: seen, (name, expr_of_pst pst)) 
          | p -> raise_explain_pst "Malformed binding in let expression" p
          in let (_, bindings) = List.fold_left_map bind_def [] defs
          in Let (bindings, expr_of_pst body)
      | Symbol "let"  , _ -> raise_expect_args 2 "let expression"

      | Symbol "cond", clauses ->  
          let pair_clause = function 
          | Node [pred; body] -> (expr_of_pst pred, expr_of_pst body)
          | p -> raise_explain_pst "Malformed clause in cond expression " p
          in Cond (List.map pair_clause clauses)

      | Symbol f, args -> Call (f, List.map expr_of_pst args)


(* ^1 tagging data is a transformation equivalent to placing a value into a functorial context. *)


let expr_of_string s = s
  |> Pstparser.pst_of_string
  |> expr_of_pst

let binding_of_pst p =
  match p with
  | Symbol _ -> TopLevelExpr (expr_of_pst p)
  | Node [] -> raise (AbstractSyntaxError "Expected binding but got '()'")
  | Node (head :: args) ->
      match head, args with
      | Symbol "define", [Symbol name; value] -> VarBinding (name, expr_of_pst value)
      | Symbol "define", [Node (Symbol name :: params); body] ->
          let rec symbols_of_pst_list = function 
          | [] -> [] 
          | Pst.Symbol s :: ss -> s :: symbols_of_pst_list ss
          | p :: _ -> raise (AbstractSyntaxError ("Expected function parameter to be a symbol but got " ^ string_of_pst p))
          in FunctionBinding { name = name; param_names = symbols_of_pst_list params; body = expr_of_pst body }
      | Symbol "define", _ -> raise (AbstractSyntaxError("This definition is malformed " ^ string_of_pst p))

      | Symbol "test", [p'] -> TestBinding (expr_of_pst p')
      | Symbol "test", _ -> AbstractSyntaxError ("This test is malformed " ^ string_of_pst p) |> raise 

      | Node _, _ -> raise (AbstractSyntaxError("Expected binding to start with a symbol but got " ^ string_of_pst p))
      | _ -> TopLevelExpr (expr_of_pst p)


let (%) f g x = f (g x)

let binding_of_string = binding_of_pst % Pstparser.pst_of_string

let bindings_of_string s =
  let p = Pstparser.pstparser_of_string s in
  let rec parse_binding_list () =
    match Pstparser.parse_pst p with
    | None -> []
    | Some pst -> binding_of_pst pst :: parse_binding_list ()
  in parse_binding_list ()
