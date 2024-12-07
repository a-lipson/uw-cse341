open Trefoil4lib
open Errors

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

let ast_error_parse_test      = ast_error_test eos 
let ast_error_binding_test    = ast_error_test bos
let ast_error_interp_test     = ast_error_test (ie0 % eos)
let runtime_error_interp_test = runtime_error_test (ie0 % eos)

(* util function tests (cannot use with autograder) *)
(* let%test "ensure_unique duplicates" = ast_error_test (Ast.ensure_unique (AbstractSyntaxError "err")) ["x"; "x"] *)
(* let%test "ensure_unique no duplicates" = Ast.ensure_unique (AbstractSyntaxError "err") ["x"; "y"] = ["x"; "y"] *)

(* expr primitive value tests *)

(* int tests *)

let%test "interpret int"          = Ast.Int 3 = ie0 (eos "3")
let%test "interpret negative int" = Ast.Int (-10) = ie0 (eos "-10")

(* bool tests *)

let%test "parse false" = Ast.Bool false = eos "false"
let%test "parse true"  = Ast.Bool true  = eos "true"

let%test "interpret true"  = Ast.Bool true  = ie0 (eos "true")
let%test "interpret false" = Ast.Bool false = ie0 (eos "false")

(* int binary operation tests *)

let%test "parse add" = Ast.Add (Ast.Int 1, Ast.Int 2) = eos "(+ 1 2)"
let%test "parse sub" = Ast.Sub (Ast.Int 1, Ast.Int 2) = eos "(- 1 2)"
let%test "parse mul" = Ast.Mul (Ast.Int 1, Ast.Int 2) = eos "(* 1 2)"
let%test "parse add partially applied" = ast_error_interp_test "(+ 1)"
let%test "parse sub partially applied" = ast_error_interp_test "(- 1)"
let%test "parse mul partially applied" = ast_error_interp_test "(* 1)"

let%test "interpret add" = Ast.Add (Ast.Int 1, Ast.Int 2) |> ie0 = Ast.Int 3
let%test "interpret sub" = Ast.Sub (Ast.Int 1, Ast.Int 2) |> ie0 = Ast.Int (-1)
let%test "interpret mul" = Ast.Mul (Ast.Int 1, Ast.Int 2) |> ie0 = Ast.Int 2
let%test "interpret add" = "(+ 1 2)" |> eos |> ie0 = Ast.Int 3   
let%test "interpret sub" = "(- 1 2)" |> eos |> ie0 = Ast.Int (-1)
let%test "interpret mul" = "(* 2 3)" |> eos |> ie0 = Ast.Int 6   
let%test "interpret add wrong types" = runtime_error_interp_test "(+ 1 true)"
let%test "interpret sub wrong types" = runtime_error_interp_test "(- 1 true)"
let%test "interpret mul wrong types" = runtime_error_interp_test "(* 1 true)"


(* if tests *)

let%test "parse if" = Ast.If (Ast.Bool true, Ast.Int 1, Ast.Int 2) = eos "(if true 1 2)"
let%test "parse if partially applied" = ast_error_interp_test "(if true)"
let%test "parse if partially applied" = ast_error_interp_test "(if true 1)"

let%test "interpret if" = Ast.If (Ast.Bool true , Ast.Int 1, Ast.Int 2) |> ie0 = Ast.Int 1
let%test "interpret if" = Ast.If (Ast.Int  0    , Ast.Int 1, Ast.Int 2) |> ie0 = Ast.Int 1
let%test "interpret if" = Ast.If (Ast.Bool false, Ast.Int 1, Ast.Int 2) |> ie0 = Ast.Int 2
(* don't want these tests since we have dynamic typing *)
(* let%test "interpret if wrong types" = runtime_error_interp_test "(if true 1 true)" *)
(* let%test "interpret if wrong types" = runtime_error_interp_test "(if true true 1)" *)
let%test "interpret if" = "(if true 0 1)"  |> eos |> ie0 = Ast.Int 0
let%test "interpret if" = "(if false 0 1)" |> eos |> ie0 = Ast.Int 1
let%test "interpret if" = "(if true 0 x)"  |> eos |> ie0 = Ast.Int 0
let%test "interpret if" = "(if 5 0 1)"     |> eos |> ie0 = Ast.Int 0


(* let tests *)

let%test "parse let" = "(let ((x 3) (y 4)) (+ x y))" |> eos = 
  Ast.Let ([("x", Ast.Int 3); ("y", Ast.Int 4)], Ast.Add (Ast.Var "x", Ast.Var "y")) 

let%test "parse let malformed" = ast_error_parse_test "(let ())"
let%test "parse let malformed" = ast_error_parse_test "(let (x 1) ())"
let%test "parse let malformed" = ast_error_parse_test "(let ((x 1)) )"
let%test "parse let malformed duplicate" = ast_error_parse_test "(let ((x 1) (x 2)) 1)"

let%test "interpret let" = "(let ((x 3)) (+ x 1))"                     |> eos |> ie0 = Ast.Int 4
let%test "interpret let" = "(let ((x 1)) (let ((x 2)) x))"             |> eos |> ie0 = Ast.Int 2
let%test "interpret let" = "(let ((x 2)) (* (let ((x 3)) x) (+ x 5)))" |> eos |> ie0 = Ast.Int 21

let%test "interpret let multi var" = Ast.Int 7 = ie0 (eos "(let ((x 3) (y 4)) (+ x y))")
let%test "interpret let no var" = Ast.Int 0 = ie0 (eos "(let () 0)")
let%test "interpret let swap" = Ast.Int 1 = ie0 (eos "(let ((x 3) (y 4)) (let ((x y) (y x)) (- x y)))")


(* environment interp tests *)

let xto3 = [("x", Ast.Int 3)]
let%test "intepret with environment"     = Ast.Int 3 = ie xto3 (eos "x")
let%test "intepret with bad environment" = 
  try ignore (ie xto3 (eos "y")); false
  with RuntimeError _ -> true

(* comment tests *)

let%test "interpret comment" = Ast.Int 3 = ie0 (eos "(+ ; comment test \n1 2)")

(* test tests *)

let%test "parse test" = "(test true)"    |> bos = Ast.TestBinding (Ast.Bool true)
let%test "parse test" = "(test (= 1 1))" |> bos = Ast.TestBinding (Ast.Eq (Ast.Int 1, Ast.Int 1))
let%test "parse test malformed" = ast_error_binding_test "(test ())"
let%test "parse test malformed" = ast_error_binding_test "(test 1 true)"

(* ensure that no exception is thrown while interpreting *)
let%test_unit "interpret test" =
  let program = "(define x 3) (test (= 3 x))" in
  program |> bsos |> ibs0 |> ignore
let%test "interpret failing test" = runtime_error_test (ibs0 % bsos) "(define x 3) (test (= 2 x))"


(* left and right (car and cdr) tests *)

let%test "interpret car" = Ast.Car (Ast.Cons (Ast.Int 1, Ast.Bool true)) |> ie0 = Ast.Int 1
let%test "interpret cdr" = Ast.Cdr (Ast.Cons (Ast.Int 1, Ast.Bool true)) |> ie0 = Ast.Bool true

let%test "interpret car" = "(car (cons 1 2))" |> eos |> ie0 = Ast.Int 1
let%test "interpret cdr" = "(cdr (cons 1 2))" |> eos |> ie0 = Ast.Int 2

let%test "interpret car nested" = Ast.Let ([( "x", Ast.Int 1 )], Ast.Car (Ast.Cons (Ast.Var "x", Ast.Bool true))) |> ie0 = Ast.Int 1
let%test "interpret car nested" = Ast.Car (Ast.Cons (Ast.Let ([("x", Ast.Int 1)], Ast.Var "x"), Ast.Nil)) |> ie0 = Ast.Int 1

(* cons tests *)

let%test "interpret cons" = Ast.Cons (Ast.Int 1, Ast.Int 2) = ie0 (eos "(cons 1 2)")

(* cons? tests *)

let%test "parse cons?" = Ast.IsCons (Ast.Cons (Ast.Int 1, Ast.Bool true)) = eos "(cons? (cons 1 true))"
let%test "parse cons? malformed" = ast_error_interp_test "(cons?)"
let%test "parse cons? malformed" = ast_error_interp_test "(cons? ())"
let%test "parse cons? malformed" = ast_error_interp_test "(cons? 1 2)"
(* wrong type input not applicable, should return false *)

let%test "interpret cons?" = Ast.IsCons (Ast.Cons (Ast.Int 1, Ast.Bool true)) |> ie0 = Ast.Bool true
let%test "interpret cons?" = Ast.IsCons (Ast.Bool true)                       |> ie0 = Ast.Bool false
let%test "interpret cons? nested" = Ast.IsCons (Ast.Cdr (Ast.Cons (Ast.Nil, Ast.Cons (Ast.Nil, Ast.Nil)))) |> ie0 = Ast.Bool true


(* nil tests *)

let%test "interpret nil" = Ast.Nil = ie0 (eos "nil")

(* nil? tests *)

let%test "parse nil?" = Ast.IsNil Ast.Nil = eos "(nil? nil)"
let%test "parse nil? malformed" = ast_error_interp_test "(nil?)"
let%test "parse nil? malformed" = ast_error_interp_test "(nil? ())"
let%test "parse nil? malformed" = ast_error_interp_test "(nil? 1 2)"
(* wrong type input not applicable, should return false *)
let%test "interpret nil?" = Ast.IsNil Ast.Nil         |> ie0 = Ast.Bool true
let%test "interpret nil?" = Ast.IsNil (Ast.Bool true) |> ie0 = Ast.Bool false
let%test "interpret nil? nested" = Ast.IsNil (Ast.Car (Ast.Cons (Ast.Nil, Ast.Int 1))) |> ie0 = Ast.Bool true


(* cond tests *)

let%test "parse cond" = "(cond (true 1))" |> eos = Ast.Cond [(Ast.Bool true, Ast.Int 1)] 
let%test "parse cond empty" = "(cond)" |> eos = Ast.Cond []

let%test "parse cond malformed" = ast_error_parse_test "(cond ())"
let%test "parse cond malformed" = ast_error_parse_test "(cond (true))"

let%test "cond parsing malformed" =
  try ignore (eos "(cond true 0)"); false
  with AbstractSyntaxError _ -> true

let%test "interpret cond basic" = ("(cond ((= 0 1) 17) ((= 0 0) 42))" |> eos |> ie0) = Ast.Int 42

let%test "interpret cond" = ("(cond (true 1))" |> eos |> ie0) = Ast.Int 1 
let%test "interpret cond" = ("(let ((x 1)) (cond ((= x 1) 1) ((= x 2) 2)))" |> eos |> ie0) = Ast.Int 1

let%test "interpret cond empty" = 
  try ignore (ie0 (eos "(cond)")); false
  with RuntimeError _ -> true

let%test "interpret cond no truthy clauses" = 
  try ignore ("(cond (false 0) ((= 0 1) 0))" |> eos |> ie0); false 
  with RuntimeError _ -> true



(* define tests *)

(* NOTE: this test is confusing*)
let%test "parse function define" = "(define (f x) (+ x 1))" |> eos = 
  Ast.Call (Ast.Var "define", [Ast.Call (Ast.Var "f", [Ast.Var "x"]); Ast.Add (Ast.Var "x", Ast.Int 1)])
let%test "parse function binding" = "(define (f x) (+ x 1))" |> bos = 
  Ast.FunctionBinding { func_name = "f"; param_names = ["x"]; body = Ast.Add (Ast.Var "x", Ast.Int 1)}

let%test "interpret define" = Ast.Int 3 = ieab0 (bsos "(define x (+ 1 2))", eos "x")


(* symbol tests *)

let%test "parse symbol" = "'a" |> eos = Ast.Symbol "a"
let%test "parse symbol empty" = ast_error_parse_test "'"

let%test "intepret symbol" = "'a" |> eos |> ie0 = Ast.Symbol "a"


(* print tests *)

let%test "parse print" = "(print 1)" |> eos = Ast.Print (Ast.Int 1) 
let%test "parse print empty" = ast_error_parse_test "(print)"

let%test "interpret print" = "(print 1)" |> eos |> ie0 = Ast.Nil 


(* closure tests *)

(* do not need parsing tests as users cannot directly write syntax to produce closures *)
let%test "interpret closure" = 
  let program =
    "(define (map f l)
        (if (cons? l) 
          (cons (f (car l)) (map f (cdr l)))
          nil))"
  in ieab0 (bsos program, eos "(map (lambda (x) (+ x 1)) (cons 1 (cons 2 nil)))") = Ast.Cons (Ast.Int 2, Ast.Cons (Ast.Int 3, Ast.Nil))

(* lambda tests *)

let%test "parse lambda" = "(lambda (x) (+ x 1))" |> eos = Ast.Lambda { rec_name = None; lambda_param_names = ["x"]; lambda_body = Ast.Add (Ast.Var "x", Ast.Int 1)} 
let%test "parse lambda missing args" = ast_error_parse_test "(lambda)"
let%test "parse lambda missing body" = ast_error_parse_test "(lambda (x))"

let%test "interpret lambda" = "((lambda (x) (+ x 1)) 1)" |> eos |> ie0 = Ast.Int 2
let%test "interpret lambda" = "(let ((f (lambda (x) (+ x 1))) (a 1)) (f a))" |> eos |> ie0 = Ast.Int 2

(* equals tests *)

let%test "parse eq" = Ast.Eq (Ast.Int 1, Ast.Int 1) = eos "(= 1 1)"
let%test "eq partially applied" = ast_error_interp_test "(= 1)"

let%test "interpret eq" = Ast.Eq (Ast.Int 1, Ast.Int 1) |> ie0 = Ast.Bool true
let%test "interpret eq" = Ast.Eq (Ast.Int 1, Ast.Int 2) |> ie0 = Ast.Bool false
(* don't want these tests since we have dynamic typing *)
(* let%test "interpret eq wrong types" = runtime_error_interp_test "(= 1 true)" *)
(* let%test "interpret eq wrong types" = runtime_error_interp_test "(= true 1)" *)
let%test "intepret equals" = "(= 3 (+ 1 2))" |> eos |> ie0 = Ast.Bool true
let%test "intepret equals" = "(= 4 (+ 1 2))" |> eos |> ie0 = Ast.Bool false 
let%test "intepret equals mixed types" = "(= 4 true)" |> eos |> ie0 = Ast.Bool false
let%test "intepret equals symbols"     = "(= 'a 'a)"  |> eos |> ie0 = Ast.Bool true

let%test "interpret equals structural equality" = "(= (cons 1 2) (cons 1 2))"          |> eos |> ie0 = Ast.Bool true
let%test "interpret equals structural equality" = "(= (cons (cons 1 1) 1) (cons 1 1))" |> eos |> ie0 = Ast.Bool false
let%test "interpret equals structural equality with closure" = 
  runtime_error_interp_test "(= (lambda (x) (x + 1)) (1))"
let%test "interpret equals structural equality with closure" = 
  runtime_error_interp_test "(= (f 1) (1))"
let%test "interpret equals structural equality with closure" = 
  runtime_error_interp_test "(= (cons (f 1) 1) (cons 1 1))"


(* match tests *)

let%test "parse match" = "(match l (nil 0) ((cons x xs) 1))" |> eos = 
  Ast.Match (Ast.Var "l", [(Ast.NilPattern, Ast.Int 0); (Ast.ConsPattern (Ast.VarPattern "x", Ast.VarPattern "xs"), Ast.Int 1)])

let%test "parse match empty" = ast_error_parse_test "(match)"
(* match can take no clauses *)
(* let%test "parse match no clauses" = ast_error_parse_test "(match x)" *)

let%test "interpret match" = true

(* TODO: add interpret match tests *)

(* COMPOUND TESTS *)

let%test "basic function" =
  let program =
    "(define (f x) (+ x 1))
     (define y (f 2))"
  in Ast.Int 3 = ieab0 (bsos program, eos "y")

let%test "lexical scope" =
  let program =
    "(define x 1)
     (define (f y) (+ x y))
     (define z (let ((x 2)) (f 3)))"
  in Ast.Int 4 = ieab0 (bsos program, eos "z")

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

let sum_binding =
  "(define (sum l)
     (if (nil? l)
       0
       (+ (car l) (sum (cdr l)))))"

let sum_cond_binding =
  "(define (sum l)
      (cond
        ((nil? l) 0)
        (true (+ (car l) (sum (cdr l))))))"

let%test "sum_countdown" = 
  Ast.Int 55 = ieab0 (bsos (countdown_binding ^ sum_binding), eos "(sum (countdown 10))")

let%test "car_cdr_countdown" =
  let expression = "(car (cdr (countdown 10)))" in
  Ast.Int 9 = ieab0 (bsos countdown_binding, eos expression)

let%test "sum cond" =
  let program = countdown_binding ^ sum_cond_binding in
  Ast.Int 55 = ieab0 (bsos program, eos "(sum (countdown 10))")

(* struct tests *)

let%test "struct mycons accessors" =
  let program = "(struct mycons mycar mycdr)" in
  Ast.Int 0 = ieab0 (bsos program, eos "(mycons-mycar (mycons 0 1))") &&
  Ast.Int 1 = ieab0 (bsos program, eos "(mycons-mycdr (mycons 0 1))")

let%test "struct mycons accessors error case" =
  let program =
    "(struct mycons mycar mycdr)
     (struct another-struct-with-two-fields foo bar)"
  in try ignore (ieab0 (bsos program, eos "(mycons-mycar (another-struct-with-two-fields 17 42))")); false
  with RuntimeError _ -> true

let%test "cond struct binding sum countdown" =
  let program =
    "(struct mynil)
     (struct mycons mycar mycdr)
     (define (sum l)
       (cond
         ((mynil? l) 0)
         ((mycons? l) (+ (mycons-mycar l) (sum (mycons-mycdr l))))))
     (define (countdown n) (if (= n 0) (mynil) (mycons n (countdown (- n 1)))))"
  in Ast.Int 55 = ieab0 (bsos program, eos "(sum (countdown 10))")

(* match tests *)

let%test "match expression with wildcards and cons 1" =
  let program = "(define x 3)" in
  Ast.Int 42 = ieab0 (bsos program, eos "(match (+ x 14) ((cons _ _) 25) (_ 42))")

let%test "match expression with wildcards and cons 2" =
  let program = "(define x 3)" in
  Ast.Int 25 = ieab0 (bsos program, eos "(match (cons (+ x 14) (+ x 15)) ((cons _ _) 25) (_ 42))")

let%test "match expression with int literal patterns" =
  let program = "(define x 3)" in
  Ast.Int 30 = ieab0 (bsos program, eos "(match (+ x 14) ((cons _ _) 25) (17 30) (_ 42))")

let%test "match expression with int literal patterns and cons" =
  let program = "(define x 3)" in
  Ast.Int 2 = ieab0 (bsos program, eos "(match (cons (+ x 14) (+ x 15)) (17 30) ((cons 17 0) 25) ((cons _ 18) 2) (_ 42))")

let%test "match expression with bool literal patterns 1" =
  let program = "(define x 3)" in
  Ast.Int 30 = ieab0 (bsos program, eos "(match (= x 3) ((cons _ _) 25) (false 17) (true 30) (_ 42))")

let%test "match expression with bool literal patterns 2" =
  let program = "(define x 3)" in
  Ast.Int 17 = ieab0 (bsos program, eos "(match (= x 4) ((cons _ _) 25) (true 30) (false 17) (_ 42))")

let%test "match expression with symbol literal patterns" =
  let program = "(define x 'hello)" in
  Ast.Int 17 = ieab0 (bsos program, eos "(match x ('world 25) ('hello 17) (true 30) (_ 42))")

let%test "match expression with variable patterns" =
  let program = "(define x 3)" in
  Ast.Int 306 = ieab0 (bsos program, eos "(match (cons (+ x 14) (+ x 15)) ((cons a b) (* a b)) (_ 42))")

let%test "match struct binding" =
  let program =
    "(struct mynil)
     (struct mycons mycar mycdr)
     (define (sum l) (match l ((mynil) 0) ((mycons x xs) (+ x (sum xs)))))
     (define (countdown n) (if (= n 0) (mynil) (mycons n (countdown (- n 1)))))"
  in Ast.Int 55 = ieab0 (bsos program, eos "(sum (countdown 10))")

let%test "match clause duplicate var pattern names" =
  let sum_with_match_error =
    "(define (sum l)
      (match l
        (nil 0)
        ((cons x x) (+ x (sum xs)))))"
  in try ignore (ib [] (bos (sum_with_match_error))); false
  with AbstractSyntaxError _ -> true
