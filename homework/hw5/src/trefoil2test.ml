open Trefoil2lib
open Errors

(* Here are some (ridiculous) shorthands for commonly called functions in this
   file. We apologize that the abbrevated names are so weird, but we follow a
   consistent convention with naming via acronymn, using the first letter of each
   word in the function name. So for example "ieab" below stands for
   "interpret_expression_after_bindings". We also use a trailing 0 to indicate
   "in the empty environment" rather than requiring an environment to be passed
   in. *)
let ie dynenv e = Interpreter.interpret_expression dynenv e
let ie0 e = ie [] e
let ib dynenv b = Interpreter.interpret_binding dynenv b
let ibs dynenv bs = Interpreter.interpret_bindings dynenv bs
let ibs0 bs = Interpreter.interpret_bindings [] bs
let eos s = Ast.expr_of_string s
let bos s = Ast.binding_of_string s
let bsos s = Ast.bindings_of_string s
let ieab dynenv bindings expr =
  Interpreter.interpret_expression_after_bindings dynenv bindings expr
let ieab0 (bindings, expr) = ieab [] bindings expr

let (%) f g x = f (g x)

let ast_error_test f s = 
  try s |> f |> ignore; false 
  with AbstractSyntaxError _ -> true

let runtime_error_test f s = 
  try s |> f |> ignore; false 
  with RuntimeError _ -> true


let%test _ = Ast.Int 3 = ie0 (eos "3")
let%test _ = Ast.Int (-10) = ie0 (eos "-10")
(* let%test "interpret_true" = Ast.Bool true = ie0 (eos "true") *)

let%test "parsing false" = Ast.Bool false = eos "false"
let%test "parsing true"  = Ast.Bool true  = eos "true"

let%test "parsing add" = Ast.Add (Ast.Int 1, Ast.Int 2) = eos "(+ 1 2)"
let%test "parsing sub" = Ast.Sub (Ast.Int 1, Ast.Int 2) = eos "(- 1 2)"
let%test "parsing mul" = Ast.Mul (Ast.Int 1, Ast.Int 2) = eos "(* 1 2)"

let%test "parsing eq" = Ast.Eq (Ast.Int 1, Ast.Int 1) = eos "(= 1 1)"

let%test "parsing if" = Ast.If (Ast.Bool true, Ast.Int 1, Ast.Int 2) = eos "(if true 1 2)"


let%test "interpret false" = Ast.Bool false = ("false" |> eos |> ie0)
let%test "interpret true"  = Ast.Bool true  = ("true"  |> eos |> ie0)

let%test "interpret eq" = Ast.Eq (Ast.Int 1, Ast.Int 1) |> ie0 = Ast.Bool true
let%test "interpret eq" = Ast.Eq (Ast.Int 1, Ast.Int 2) |> ie0 = Ast.Bool false

let%test "interpret if" = Ast.If (Ast.Bool true , Ast.Int 1, Ast.Int 2) |> ie0 = Ast.Int 1
let%test "interpret if" = Ast.If (Ast.Int  0    , Ast.Int 1, Ast.Int 2) |> ie0 = Ast.Int 1
let%test "interpret if" = Ast.If (Ast.Bool false, Ast.Int 1, Ast.Int 2) |> ie0 = Ast.Int 2

(* let%test "interpret add" = Ast.Add (Ast.Int 1, Ast.Int 2) |> ie0 = Ast.Int 3 *)
(* let%test "interpret sub" = Ast.Sub (Ast.Int 1, Ast.Int 2) |> ie0 = Ast.Int (-1) *)
(* let%test "interpret mul" = Ast.Mul (Ast.Int 1, Ast.Int 2) |> ie0 = Ast.Int 2 *)

(* test environment *)
let xto3 = [("x", Ast.Int 3)]

let%test _ = Ast.Int 3 = ie xto3 (eos "x")

let%test _ = try ignore (ie xto3 (eos "y")); false
             with RuntimeError _ -> true
let%test _ = Ast.Int 3 = ie0 (eos "(+ 1 2)")


let ast_error_interp_test = ast_error_test (ie0 % eos)

let%test "test add abstract syntax error" = ast_error_interp_test "(+ 1)"
let%test "test sub abstract syntax error" = ast_error_interp_test "(- 1)"
let%test "test mul abstract syntax error" = ast_error_interp_test "(* 1)"

let%test "test eq abstract syntax error" = ast_error_interp_test "(= 1)"

let%test "test if abstract syntax error" = ast_error_interp_test "(if true)"
let%test "test if abstract syntax error" = ast_error_interp_test "(if true 1)"

let runtime_error_interp_test = runtime_error_test (ie0 % eos)

let%test "test add wrong types" = runtime_error_interp_test "(+ 1 true)"
let%test "test sub wrong types" = runtime_error_interp_test "(- 1 true)"
let%test "test mul wrong types" = runtime_error_interp_test "(* 1 true)"

let%test "test eq wrong types" = runtime_error_interp_test "(= 1 true)"
let%test "test eq wrong types" = runtime_error_interp_test "(= true 1)"

(* don't want these tests since we have dynamic typing *)
(* let%test "test_if_wrong_types" = runtime_error_interp_test "(if true 1 true)" *)
(* let%test "test_if_wrong_types" = runtime_error_interp_test "(if true true 1)" *)

let%test "interpret sub" = Ast.Int (-1) = ie0 (eos "(- 1 2)")
let%test "interpret mul" = Ast.Int 6 = ie0 (eos "(* 2 3)")

let%test _ = Ast.Bool true  = ie0 (eos "(= 3 (+ 1 2))")
let%test _ = Ast.Bool false = ie0 (eos "(= 4 (+ 1 2))")

(* let%test _ = runtime_error_interp_test "(= 4 true)"  *)

let%test _ = Ast.Int 0 = ie0 (eos "(if true 0 1)" )
let%test _ = Ast.Int 1 = ie0 (eos "(if false 0 1)")
let%test _ = Ast.Int 0 = ie0 (eos "(if true 0 x)" )
let%test _ = Ast.Int 0 = ie0 (eos "(if 5 0 1)"    )

let%test "parsing let" = 
  "(let ((x 3)) (+ x 1))" |> eos = 
    Ast.Let ("x", Ast.Int 3, Ast.Add (Ast.Var "x", Ast.Int 1)) 

let ast_error_parse_test = ast_error_test eos 

let%test "parsing let malformed" = ast_error_parse_test "(let ())"
let%test "parsing let malformed" = ast_error_parse_test "(let (x 1) ())"
let%test "parsing let malformed" = ast_error_parse_test "(let ((x 1)))"

let%test "test let 1" = Ast.Int 4  = ie0 (eos "(let ((x 3)) (+ x 1))"                    )
let%test "test let 2" = Ast.Int 2  = ie0 (eos "(let ((x 1)) (let ((x 2)) x))"            )
let%test "test let 3" = Ast.Int 21 = ie0 (eos "(let ((x 2)) (* (let ((x 3)) x) (+ x 5)))")

let%test _ = Ast.Int 3 = ie0 (eos "(+ ; comment test \n1 2)")
let%test _ = Ast.Nil = ie0 (eos "nil")
let%test _ = Ast.Cons (Ast.Int 1, Ast.Int 2) = ie0 (eos "(cons 1 2)")
let%test _ = Ast.Int 1 = ie0 (eos "(car (cons 1 2))")
let%test _ = Ast.Int 2 = ie0 (eos "(cdr (cons 1 2))")
let%test _ = Ast.Int 3 = ieab0 (bsos "(define x (+ 1 2))", eos "x")

let%test "test binding parsing" = "(test true)" |> bos = 
  Ast.TestBinding (Ast.Bool true)
let%test "test binding parsing" = "(test (= 1 1))" |> bos = 
  Ast.TestBinding (Ast.Eq (Ast.Int 1, Ast.Int 1))

let ast_error_binding_test = ast_error_test bos

let%test "test binding parsing malformed" = ast_error_binding_test "(test ())"
let%test "test binding parsing malformed" = ast_error_binding_test "(test 1 true)"

(* ensure that no exception is thrown while interpreting *)
let%test_unit "simple test binding" =
  let program = "(define x 3) (test (= 3 x))" in
  program |> bsos |> ibs0 |> ignore

let%test "failing test binding" = runtime_error_test (ibs0 % bsos) "(define x 3) (test (= 2 x))"

let%test "intepret car" = Ast.Car (Ast.Cons (Ast.Int 1, Ast.Bool true)) |> ie0 = Ast.Int 1
let%test "intepret cdr" = Ast.Cdr (Ast.Cons (Ast.Int 1, Ast.Bool true)) |> ie0 = Ast.Bool true

let%test "intepret car nested" = Ast.Let ("x", Ast.Int 1, Ast.Car (Ast.Cons (Ast.Var "x", Ast.Bool true))) |> ie0 = Ast.Int 1
let%test "intepret car nested" = Ast.Car (Ast.Cons (Ast.Let ("x", Ast.Int 1, Ast.Var "x"), Ast.Nil)) |> ie0 = Ast.Int 1


let%test "parsing cons?" = Ast.IsCons (Ast.Cons (Ast.Int 1, Ast.Bool true)) = eos "(cons? (cons 1 true))"
let%test "intepret cons?" = Ast.IsCons (Ast.Cons (Ast.Int 1, Ast.Bool true)) |> ie0 = Ast.Bool true
let%test "intepret cons?" = Ast.IsCons (Ast.Bool true)                       |> ie0 = Ast.Bool false

let%test "intepret cons? nested" = Ast.IsCons (Ast.Cdr (Ast.Cons (Ast.Nil, Ast.Cons (Ast.Nil, Ast.Nil)))) |> ie0 = Ast.Bool true

let%test "cons? malformed" = ast_error_interp_test "(cons?)"
let%test "cons? malformed" = ast_error_interp_test "(cons? ())"
let%test "cons? malformed" = ast_error_interp_test "(cons? 1 2)"
(* wrong type input not applicable, should return false *)

let%test "parsing nil?" = Ast.IsNil Ast.Nil = eos "(nil? nil)"
let%test "intepret nil?" = Ast.IsNil Ast.Nil         |> ie0 = Ast.Bool true
let%test "intepret nil?" = Ast.IsNil (Ast.Bool true) |> ie0 = Ast.Bool false

let%test "intepret nil? nested" = Ast.IsNil (Ast.Car (Ast.Cons (Ast.Nil, Ast.Int 1))) |> ie0 = Ast.Bool true

let%test "nil? malformed" = ast_error_interp_test "(nil?)"
let%test "nil? malformed" = ast_error_interp_test "(nil? ())"
let%test "nil? malformed" = ast_error_interp_test "(nil? 1 2)"
(* wrong type input not applicable, should return false *)

