include Ast_types
open Errors

(* last stage of parser: converts pst to expr *)
let rec expr_of_pst p =
  let raise_expect_args n info = raise (AbstractSyntaxError (info ^ " expects " ^ string_of_int n ^ " args but got " ^ Pst.string_of_pst p))
  (*
  let nop sym n f args = 
    if List.length args = n then f (List.map expr_of_pst args)
    else raise_expect_args n ("operator " ^ sym)
  in let op    sym c = nop sym 1 (fun [x]   -> c x  ) (* not exhaustive pattern *)
  in let binop sym c = nop sym 2 (fun [x;y] -> c x y)
  *)
  in let op    sym c = function (* args = match args with *)
    | [p] -> c (expr_of_pst p)
    | _ -> raise_expect_args 1 ("operator " ^ sym)
  in let binop sym c = function
    | [left; right] -> c (expr_of_pst left) (expr_of_pst right)
    | _ -> raise_expect_args 2 ("operator " ^ sym)
  in match p with
  | Pst.Symbol sym -> begin
     try
       Int (int_of_string sym)
     with
       Failure _ ->
       match sym with
       | "true"  -> Bool true
       | "false" -> Bool false
       | "nil"   -> Nil
       | _ -> Var sym
    end
  | Pst.Node [] -> raise (AbstractSyntaxError "Expected expression but got '()'")
  | Pst.Node (head :: args) ->
      match head, args with
      | Pst.Node _, _ -> raise (AbstractSyntaxError ("Expression forms must start with a symbol, but got " ^ Pst.string_of_pst head))
      | Pst.Symbol "car"  , args -> op "car"   (fun x -> Car    x) args (* imagine if constructors were curried functions... *)
      | Pst.Symbol "cdr"  , args -> op "cdr"   (fun x -> Cdr    x) args
      | Pst.Symbol "cons?", args -> op "cons?" (fun x -> IsCons x) args
      | Pst.Symbol "nil?" , args -> op "nil?"  (fun x -> IsNil  x) args
      | Pst.Symbol "+"   , args -> binop "+"    (fun x y -> Add  (x, y)) args
      | Pst.Symbol "-"   , args -> binop "-"    (fun x y -> Sub  (x, y)) args
      | Pst.Symbol "*"   , args -> binop "*"    (fun x y -> Mul  (x, y)) args
      | Pst.Symbol "="   , args -> binop "="    (fun x y -> Eq   (x, y)) args
      | Pst.Symbol "cons", args -> binop "cons" (fun l r -> Cons (l, r)) args
      | Pst.Symbol "if", [branch; thn; els] -> If (expr_of_pst branch, expr_of_pst thn, expr_of_pst els)
      | Pst.Symbol "if", _                  -> raise_expect_args 3 "'if' special form" 
      | Pst.Symbol "let", [Pst.Node [Pst.Node [Pst.Symbol x; e1]]; e2] -> Let ([(x, expr_of_pst e1)], expr_of_pst e2)
      (* | Pst.Symbol "let", [_; _; _] -> AbstractSyntaxError "let exprects a variable binding" |> raise *)
      | Pst.Symbol "let", _                                            -> raise_expect_args 2 "let expression"
      | Pst.Symbol f, _ -> raise (AbstractSyntaxError ("Unknown operator " ^ f))


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

let binding_of_string s = s 
  |> Pstparser.pst_of_string
  |> binding_of_pst

let bindings_of_string s =
  let p = Pstparser.pstparser_of_string s in
  let rec parse_binding_list () =
    match Pstparser.parse_pst p with
    | None -> []
    | Some pst -> 
        binding_of_pst pst :: parse_binding_list ()
  in parse_binding_list ()
