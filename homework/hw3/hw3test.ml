(* CSE 341, Homework 3 Tests *)
open Hw3
open Hw3types

(* Write your tests for Homework 3 in this file.
 * As you work on implementing the homework functions:
 *   1. Implement the changes in hw3.ml
 *   2. Run `dune build` to make sure everything compiles
 *   3. Add test scenarios
 *   4. Run `dune test` to build and run your tests
 * If working correctly, `dune test` will complete with no error messages.
 *)

let%test "search some" = search_exn [1; 3; 5; 3; 1] 3 = 1
let%test "search first" = search_exn [1; 3; 5; 3; 1] 1 = 0
let%test "search none" = 
  try
    let _ = search_exn [] 5 in
    false
  with
  | NotFound -> true
  | _ -> false


let%test "search some" = search [1; 3; 5; 3; 1] 3 = Some 1
let%test "search first" = search [1; 3; 5; 3; 1] 1 = Some 0
let%test "search none" = search [] 1 = None

let%test "add id" = add Zero Zero = Zero 
let%test "add left" = add (Succ Zero) Zero = Succ Zero
let%test "add right" = add Zero (Succ Zero) = Succ Zero
let%test "add some" = add (Succ (Succ Zero)) (Succ Zero) = Succ (Succ (Succ Zero))

let%test "to_int id" = to_int Zero = 0 
let%test "to_int some" = to_int (Succ Zero) = 1

let%test "from_int id" = from_int 0 = Zero 
let%test "from_int some" = from_int 1 = Succ Zero
let%test "from_int negative" = 
  try
    let _ = from_int ( -1 ) in false
  with
  | NegativeInput -> true
  | _ -> false

let%test "naturals type expensive operation" = to_int (add (from_int 123456789) (from_int 123456789)) = 246913578

let%test "only_lowercase some" = only_lowercase ["hello"; "World"; "Hello"; "world"] = ["hello"; "world"]

let%test "longest_string1 empty" = longest_string1 [] = ""
let%test "longest_string1 some" = longest_string1 ["one"; "two"; "three"; "four"; "eight"] = "three"

let%test "longest_string2 empty" = longest_string2 [] = ""
let%test "longest_string2 some" = longest_string2 ["one"; "two"; "three"; "four"; "eight"] = "eight"

let%test "longest_string3 empty" = longest_string3 [] = ""
let%test "longest_string3 some" = longest_string3 ["one"; "two"; "three"; "four"; "eight"] = "three"

let%test "longest_string4 empty" = longest_string4 [] = ""
let%test "longest_string4 some" = longest_string4 ["one"; "two"; "three"; "four"; "eight"] = "eight"

let%test "longest_lowercase empty" = longest_lowercase [] = ""
let%test "longest_lowercase some" = longest_lowercase ["hi"; "World"; "hello"; "world"] = "hello"

let%test "caps_no_X_string empty" = caps_no_X_string "" = ""
let%test "caps_no_X_string some" = caps_no_X_string "aBxXXxDdx" = "ABDD"

let%test "first_answer some" = first_answer (fun s -> if s = 'a' then Some s else None) ['a'; 'b'] = 'a'
let%test "first_answer none" = 
  try let _ = first_answer (fun s -> if s = 'c' then Some s else None) ['a'; 'b'] in false 
  with 
  | NoAnswer -> true 
  | _ -> false
let%test "first_answer empty" = 
  try let _ = first_answer Option.some [] in false
  with
  | NoAnswer -> true
  | _ -> false

let%test "all_answers empty" = all_answers Fun.id [] = Some []
let%test "all_answers some" = all_answers (fun x -> Option.some (x::[])) ['a'; 'b'] = Some ['a'; 'b']


let%test "count_wildcards none" = count_wildcards UnitP = 0
let%test "count_wildcards one" = count_wildcards WildcardP = 1
let%test "count_wildcards some" = count_wildcards (TupleP [WildcardP; ConstructorP ("", WildcardP)]) = 2

let%test "count_wild_and_variable_lengths none" = count_wild_and_variable_lengths UnitP = 0
let%test "count_wild_and_variable_lengths one wild" = count_wild_and_variable_lengths WildcardP = 1
let%test "count_wild_and_variable_lengths one var" = count_wild_and_variable_lengths (VariableP "var") = String.length "var"
let%test "count_wild_and_variable_lengths some" = count_wild_and_variable_lengths (TupleP [WildcardP; VariableP "var"]) = 1 + String.length "var"

let%test "count_a_var not variable" = count_a_var "" UnitP = 0
let%test "count_a_var none" = count_a_var "var" (VariableP "val") = 0
let%test "count_a_var one" = count_a_var "var" (VariableP "var") = 1 
let%test "count_a_var some" = count_a_var "var" (TupleP [ConstructorP ("var", VariableP "var"); VariableP "var"]) = 2

let%test "check_pat not variable" = check_pat UnitP = true 
let%test "check_pat one" = check_pat (VariableP "var") = true 
let%test "check_pat some" = check_pat (TupleP [ConstructorP ("var", VariableP "val"); VariableP "var"]) = true
let%test "check_pat bad" = check_pat (TupleP [ConstructorP ("var", VariableP "var"); VariableP "var"]) = false



