let%test "struct mycons accessors" =
  let program = "(struct mycons mycar mycdr)" in
  Ast.Int 0 = ieab0 (bsos program, eos "(mycons-mycar (mycons 0 1))") &&
  Ast.Int 1 = ieab0 (bsos program, eos "(mycons-mycdr (mycons 0 1))")

let%test "struct mycons accessors error case" =
  let program =
    "(struct mycons mycar mycdr)
     (struct another-struct-with-two-fields foo bar)"
  in
  try
    ignore (ieab0 (bsos program, eos "(mycons-mycar (another-struct-with-two-fields 17 42))"));
    false
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
  in
  Ast.Int 55 = ieab0 (bsos program, eos "(sum (countdown 10))")



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
  in
  Ast.Int 55 = ieab0 (bsos program, eos "(sum (countdown 10))")


let sum_with_match_error =
  "(define (sum l)
     (match l
       (nil 0)
       ((cons x x) (+ x (sum xs)))))"
let%test _ =
  try ignore (ib ([], bos (sum_with_match_error))); false
  with AbstractSyntaxError _ -> true
