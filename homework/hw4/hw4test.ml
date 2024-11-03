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

(* 4 *)
let rec ones = Stream (fun () -> (1,ones))

let nats = 
  let rec go start = (start, Stream (fun () -> go (start + 1)))
  in Stream (fun () -> go 0)

let%test _ = stream_first_k_such_that (fun x -> true) 0 ones = []
let%test _ = stream_first_k_such_that (fun x -> x = 1) 2 ones = [1;1]
let%test _ = stream_first_k_such_that (fun x -> x mod 2 = 0) 3 nats = [0;2;4]

(* 5 *)
let%test _ = stream_first_k_such_that (fun x -> x > 0) 6 funny_number_stream = [1;2;3;4;5;7]

(* 6 *)
let%test _ = stream_first_k_such_that (fun x -> true) 3 foo_then_bar = ["foo";"bar";"foo"] 

(* 7 *)
let%test _ = stream_first_k_such_that 
  (fun x -> true) 2 (stream_pair_all_with_one foo_then_bar) = [(1,"foo");(1,"bar")]

(* 8 *)
let%test _ = stream_first_k_such_that 
  (fun x -> true) 8 (cycle_lists [1;2;3] ["a";"b"]) =
  [(1, "a"); (2, "b"); (3, "a"); (1, "b"); (2, "a"); (3, "b"); (1, "a"); (2, "b")]

