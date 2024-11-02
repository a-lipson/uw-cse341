open Hw4
open Hw4types

(* 1 *)
let%test _ = sequence 2 3 11 = [3; 5; 7; 9; 11]
let%test _ = sequence 3 3 8 = [3; 6]
let%test _ = sequence 1 3 2 = []

(* 2 *)
let%test _ = string_append_map [] "foo" = []
let%test _ = string_append_map ["a";"b";"c"] "x" = ["ax";"bx";"cx"]

(* 3 *)
let%test _ = try 
  let _ = list_nth_mod [] (-1) in false with 
  | NegativeInput -> true
  | _ -> false
let%test _ = try 
  let _ = list_nth_mod [] 1 in false with 
  | EmptyList -> true
  | _ -> false
let%test _ = list_nth_mod ["a";"b"] 2 = "a"
let%test _ = list_nth_mod ["a";"b"] 4 = "a"
let%test _ = list_nth_mod ["a";"b"] 1 = "b"
