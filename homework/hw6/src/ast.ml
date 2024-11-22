include Ast_types
open Errors

(* last stage of parser: converts pst to expr *)
let rec expr_of_pst p =
  let raise_expect_args n info = 
    raise (AbstractSyntaxError (info ^ " expects " ^ string_of_int n ^ " args but got " ^ Pst.string_of_pst p))
  in let raise_detail_expr_from_pst p info = 
    raise (AbstractSyntaxError (info ^ (p |> expr_of_pst |> string_of_expr)))
  in let op    sym c = function (* args = match args with *)
    | [p] -> c (expr_of_pst p)
    | _   -> raise_expect_args 1 ("operator " ^ sym)
  in let binop sym c = function
    | [left; right] -> c (expr_of_pst left) (expr_of_pst right)
    | _   -> raise_expect_args 2 ("operator " ^ sym)

  in match p with
  | Pst.Symbol sym -> begin
     try Int (int_of_string sym) with
       Failure _ ->
       match sym with
       | "true"  -> Bool true
       | "false" -> Bool false
       | "nil"   -> Nil
       | _       -> Var sym
    end

  | Pst.Node [] -> raise (AbstractSyntaxError "Expected expression but got '()'")
  | Pst.Node (head :: args) ->
      match head, args with
      | Pst.Node _, _ -> raise (AbstractSyntaxError ("Expression forms must start with a symbol, but got " ^ Pst.string_of_pst head))

      | Pst.Symbol "car"  , args -> op    "car"   (fun x   -> Car       x) args (* imagine if constructors were curried functions... *)
      | Pst.Symbol "cdr"  , args -> op    "cdr"   (fun x   -> Cdr       x) args
      | Pst.Symbol "cons?", args -> op    "cons?" (fun x   -> IsCons    x) args
      | Pst.Symbol "nil?" , args -> op    "nil?"  (fun x   -> IsNil     x) args
      | Pst.Symbol "+"    , args -> binop "+"     (fun x y -> Add  (x, y)) args
      | Pst.Symbol "-"    , args -> binop "-"     (fun x y -> Sub  (x, y)) args
      | Pst.Symbol "*"    , args -> binop "*"     (fun x y -> Mul  (x, y)) args
      | Pst.Symbol "="    , args -> binop "="     (fun x y -> Eq   (x, y)) args
      | Pst.Symbol "cons" , args -> binop "cons"  (fun l r -> Cons (l, r)) args

      | Pst.Symbol "if"   , [branch; thn; els] -> If (expr_of_pst branch, expr_of_pst thn, expr_of_pst els)
      | Pst.Symbol "if"   , _                  -> raise_expect_args 3 "if expression" 

      | Pst.Symbol "let"  , [Pst.Node defs; body] -> 
          (* could catch duplicate error prior to parsing all pst defs *)
          let bind = function
            | Pst.Node [Pst.Symbol name; pst] -> 
                (* let _ = print_endline ("attempting to bind " ^ name ^ " with " ^ (pst |> expr_of_pst |> string_of_expr)) in *)
                (name, expr_of_pst pst)
            | p -> raise_detail_expr_from_pst p "Malformed binding in let expression"
          in let has_dups l = 
            let sorted = List.sort_uniq String.compare (List.map fst l) in
            List.length sorted < List.length l
          (* in let rec bindings acc = function  *)
          (*   | []  ->  *)
          (*       let _ = print_endline ("returning bindings") in  *)
          (*       acc  *)
          (*   | def :: defs  -> let (name, expr) as b = bind def in *)
          (*     if not (List.exists (fun (n, _) -> n = name) acc)  *)
          (*     then  *)
          (*       let _ = print_endline ("adding " ^ fst b ^ show_expr (snd b)) in *)
          (*       bindings (b :: acc) defs *)
          (*     else  *)
          (*       raise_detail_expr_from_pst def "Duplciate binding in let expression " *)
          in let bindings = List.map bind defs in 
          if has_dups bindings then raise (AbstractSyntaxError ("Duplicate binding in let expression " ^ Pst.string_of_pst p))
          else Let (bindings, expr_of_pst body)
      | Pst.Symbol "let"  , _ -> raise_expect_args 2 "let expression"

      | Pst.Symbol "cond" , [] ->  raise (AbstractSyntaxError "not implemented")
      | Pst.Symbol "cond" , _ -> raise (AbstractSyntaxError "not implemented")

      | Pst.Symbol f, [] ->  raise (AbstractSyntaxError "not implemented")

      | Pst.Symbol s, _ -> raise (AbstractSyntaxError ("Unknown symbol " ^ s))


let expr_of_string s = s
  |> Pstparser.pst_of_string
  |> expr_of_pst

let binding_of_pst p =
  match p with
  | Pst.Symbol _ -> TopLevelExpr (expr_of_pst p)
  | Pst.Node [] -> raise (AbstractSyntaxError "Expected binding but got '()'")
  | Pst.Node (head :: args) ->
      match head, args with
      | Pst.Symbol "define", [Pst.Symbol lhs_var; rhs] -> VarBinding (lhs_var, expr_of_pst rhs)
      | Pst.Symbol "define", _ -> raise (AbstractSyntaxError("This definition is malformed " ^ Pst.string_of_pst p))

      | Pst.Symbol "test", [p'] -> TestBinding (expr_of_pst p')
      | Pst.Symbol "test", _ -> AbstractSyntaxError ("This test is malformed " ^ Pst.string_of_pst p) |> raise 

      | Pst.Node _, _ -> raise (AbstractSyntaxError("Expected binding to start with a symbol but got " ^ Pst.string_of_pst p))
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
