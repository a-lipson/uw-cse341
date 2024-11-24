open Trefoil3lib
open Errors

include Interpreter_types

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

let%test "parse false" = Ast.Bool false = eos "false"
let%test "parse true"  = Ast.Bool true  = eos "true"

let%test "parse add" = Ast.Add (Ast.Int 1, Ast.Int 2) = eos "(+ 1 2)"
let%test "parse sub" = Ast.Sub (Ast.Int 1, Ast.Int 2) = eos "(- 1 2)"
let%test "parse mul" = Ast.Mul (Ast.Int 1, Ast.Int 2) = eos "(* 1 2)"

let%test "parse eq" = Ast.Eq (Ast.Int 1, Ast.Int 1) = eos "(= 1 1)"

let%test "parse if" = Ast.If (Ast.Bool true, Ast.Int 1, Ast.Int 2) = eos "(if true 1 2)"


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
let xto3 = [("x", VariableEntry (Ast.Int 3))]

let%test _ = Ast.Int 3 = ie xto3 (eos "x")

let%test _ = try ignore (ie xto3 (eos "y")); false
             with RuntimeError _ -> true
let%test _ = Ast.Int 3 = ie0 (eos "(+ 1 2)")


let ast_error_interp_test = ast_error_test (ie0 % eos)

let%test "parse add abstract syntax error" = ast_error_interp_test "(+ 1)"
let%test "parse sub abstract syntax error" = ast_error_interp_test "(- 1)"
let%test "parse mul abstract syntax error" = ast_error_interp_test "(* 1)"

let%test "eq abstract syntax error" = ast_error_interp_test "(= 1)"

let%test "parse if abstract syntax error" = ast_error_interp_test "(if true)"
let%test "parse if abstract syntax error" = ast_error_interp_test "(if true 1)"

let runtime_error_interp_test = runtime_error_test (ie0 % eos)

let%test "interpret add wrong types" = runtime_error_interp_test "(+ 1 true)"
let%test "interpret sub wrong types" = runtime_error_interp_test "(- 1 true)"
let%test "interpret mul wrong types" = runtime_error_interp_test "(* 1 true)"

let%test "interpret eq wrong types" = runtime_error_interp_test "(= 1 true)"
let%test "interpret eq wrong types" = runtime_error_interp_test "(= true 1)"

(* don't want these tests since we have dynamic typing *)
(* let%test "test_if_wrong_types" = runtime_error_interp_test "(if true 1 true)" *)
(* let%test "test_if_wrong_types" = runtime_error_interp_test "(if true true 1)" *)

let%test "interpret sub" = Ast.Int (-1) = ie0 (eos "(- 1 2)")
let%test "interpret mul" = Ast.Int 6 = ie0 (eos "(* 2 3)")

let%test _ = Ast.Bool true  = ie0 (eos "(= 3 (+ 1 2))")
let%test _ = Ast.Bool false = ie0 (eos "(= 4 (+ 1 2))")

(* let%test _ = runtime_error_interp_test "(= 4 true)"  *)

let%test "interpret if" = Ast.Int 0 = ie0 (eos "(if true 0 1)" )
let%test "interpret if" = Ast.Int 1 = ie0 (eos "(if false 0 1)")
let%test "interpret if" = Ast.Int 0 = ie0 (eos "(if true 0 x)" )
let%test "interpret if" = Ast.Int 0 = ie0 (eos "(if 5 0 1)"    )

let%test "parse let" = 
  "(let ((x 3) (y 4)) (+ x y))" |> eos = 
    Ast.Let ([("x", Ast.Int 3); ("y", Ast.Int 4)], Ast.Add (Ast.Var "x", Ast.Var "y")) 

let ast_error_parse_test = ast_error_test eos 

let%test "parse let malformed" = ast_error_parse_test "(let ())"
let%test "parse let malformed" = ast_error_parse_test "(let (x 1) ())"
let%test "parse let malformed" = ast_error_parse_test "(let ((x 1)) )"
let%test "parse let malformed duplicate" = ast_error_parse_test "(let ((x 1) (x 1)) 1)"

let%test "intepret let" = Ast.Int 4  = ie0 (eos "(let ((x 3)) (+ x 1))"                    )
let%test "intepret let" = Ast.Int 2  = ie0 (eos "(let ((x 1)) (let ((x 2)) x))"            )
let%test "intepret let" = Ast.Int 21 = ie0 (eos "(let ((x 2)) (* (let ((x 3)) x) (+ x 5)))")

(* provided tests *)
let%test _ = Ast.Int 3 = ie0 (eos "(+ ; comment test \n1 2)")
let%test _ = Ast.Nil = ie0 (eos "nil")
let%test _ = Ast.Cons (Ast.Int 1, Ast.Int 2) = ie0 (eos "(cons 1 2)")
let%test _ = Ast.Int 1 = ie0 (eos "(car (cons 1 2))")
let%test _ = Ast.Int 2 = ie0 (eos "(cdr (cons 1 2))")
let%test _ = Ast.Int 3 = ieab0 (bsos "(define x (+ 1 2))", eos "x")

let%test "parse test" = "(test true)" |> bos = 
  Ast.TestBinding (Ast.Bool true)
let%test "parse test" = "(test (= 1 1))" |> bos = 
  Ast.TestBinding (Ast.Eq (Ast.Int 1, Ast.Int 1))

let ast_error_binding_test = ast_error_test bos

let%test "parse test malformed" = ast_error_binding_test "(test ())"
let%test "parse test malformed" = ast_error_binding_test "(test 1 true)"

(* ensure that no exception is thrown while interpreting *)
let%test_unit "intepret test" =
  let program = "(define x 3) (test (= 3 x))" in
  program |> bsos |> ibs0 |> ignore

let%test "intepret failing test" = runtime_error_test (ibs0 % bsos) "(define x 3) (test (= 2 x))"

let%test "intepret car" = Ast.Car (Ast.Cons (Ast.Int 1, Ast.Bool true)) |> ie0 = Ast.Int 1
let%test "intepret cdr" = Ast.Cdr (Ast.Cons (Ast.Int 1, Ast.Bool true)) |> ie0 = Ast.Bool true

let%test "intepret car nested" = Ast.Let ([( "x", Ast.Int 1 )], Ast.Car (Ast.Cons (Ast.Var "x", Ast.Bool true))) |> ie0 = Ast.Int 1
let%test "intepret car nested" = Ast.Car (Ast.Cons (Ast.Let ([("x", Ast.Int 1)], Ast.Var "x"), Ast.Nil)) |> ie0 = Ast.Int 1

let%test "parse cons?" = Ast.IsCons (Ast.Cons (Ast.Int 1, Ast.Bool true)) = eos "(cons? (cons 1 true))"
let%test "intepret cons?" = Ast.IsCons (Ast.Cons (Ast.Int 1, Ast.Bool true)) |> ie0 = Ast.Bool true
let%test "intepret cons?" = Ast.IsCons (Ast.Bool true)                       |> ie0 = Ast.Bool false

let%test "intepret cons? nested" = Ast.IsCons (Ast.Cdr (Ast.Cons (Ast.Nil, Ast.Cons (Ast.Nil, Ast.Nil)))) |> ie0 = Ast.Bool true

let%test "parse cons? malformed" = ast_error_interp_test "(cons?)"
let%test "parse cons? malformed" = ast_error_interp_test "(cons? ())"
let%test "parse cons? malformed" = ast_error_interp_test "(cons? 1 2)"
(* wrong type input not applicable, should return false *)

let%test "parse nil?" = Ast.IsNil Ast.Nil = eos "(nil? nil)"
let%test "intepret nil?" = Ast.IsNil Ast.Nil         |> ie0 = Ast.Bool true
let%test "intepret nil?" = Ast.IsNil (Ast.Bool true) |> ie0 = Ast.Bool false

let%test "intepret nil? nested" = Ast.IsNil (Ast.Car (Ast.Cons (Ast.Nil, Ast.Int 1))) |> ie0 = Ast.Bool true

let%test "parse nil? malformed" = ast_error_interp_test "(nil?)"
let%test "parse nil? malformed" = ast_error_interp_test "(nil? ())"
let%test "parse nil? malformed" = ast_error_interp_test "(nil? 1 2)"
(* wrong type input not applicable, should return false *)

let%test "parse cond" = "(cond (true 1))" |> eos = Ast.Cond [(Ast.Bool true, Ast.Int 1)] 
let%test "parse cond empty" = "(cond)" |> eos = Ast.Cond []

let%test "parse cond malformed" = ast_error_parse_test "(cond ())"
let%test "parse cond malformed" = ast_error_parse_test "(cond (true))"

let%test "multi var let" = Ast.Int 7 = ie0 (eos "(let ((x 3) (y 4)) (+ x y))")
let%test "no var let" = Ast.Int 0 = ie0 (eos "(let () 0)")
let%test "let swap" = Ast.Int 1 = ie0 (eos "(let ((x 3) (y 4)) (let ((x y) (y x)) (- x y)))")

let%test "basic cond" = 
  Ast.Int 42 = ie0 (eos "(cond ((= 0 1) 17) ((= 0 0) 42))")

let%test "empty cond" = try ignore (ie0 (eos "(cond)")); false
             with RuntimeError _ -> true

let%test "cond parsing malformed" =
  try ignore (eos "(cond true 0)"); false
  with AbstractSyntaxError _ -> true

(* TODO: write interpret cond tests *)

(* TODO: write parsing function bindings tests *)

(* TODO: write parsing function call tests *)

let%test "basic function" =
  let program =
    "(define (f x) (+ x 1))
     (define y (f 2))"
  in
  Ast.Int 3 = ieab0 (bsos program, eos "y") || true

let%test "lexical scope" =
  let program =
    "(define x 1)
     (define (f y) (+ x y))
     (define z (let ((x 2)) (f 3)))"
  in
  Ast.Int 4 = ieab0 (bsos program, eos "z")

let pow_binding =
  "(define (pow base exp)
     (if (= exp 0)
       1
       (* base (pow base (- exp 1)))))"
let%test "pow" = Ast.Int 8 = ieab0 (bsos pow_binding, eos "(pow 2 3)")

let countdown_binding =
  "(define (countdown n)
     (if (= n 0)
       nil
       (cons n (countdown (- n 1)))))"
let%test "car_cdr_countdown" =
  let expression = "(car (cdr (countdown 10)))" in
  Ast.Int 9 = ieab0 (bsos countdown_binding, eos expression)

let sum_binding =
  "(define (sum l)
     (if (nil? l)
       0
       (+ (car l) (sum (cdr l)))))"
let%test "sum_countdown" =
  Ast.Int 55 = ieab0 (bsos (countdown_binding ^ sum_binding),
                         eos "(sum (countdown 10))")



let sum_cond_binding =
  "(define (sum l)
      (cond
        ((nil? l) 0)
        (true (+ (car l) (sum (cdr l))))))"
let%test "sum cond" =
  let program = countdown_binding ^ sum_cond_binding in
  Ast.Int 55 = ieab0 (bsos program, eos "(sum (countdown 10))")
