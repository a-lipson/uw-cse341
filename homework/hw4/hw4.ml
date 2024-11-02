open Hw4types

(* assume spacing postive *)
let rec sequence spacing low high =
  if low > high then []
  else low :: sequence spacing (low + spacing) high

let string_append_map xs suffix = List.map (Fun.flip (^) suffix) xs 

let list_nth_mod xs n = 
  if n < 0 then raise NegativeInput 
  else if xs = [] then raise EmptyList 
  else List.nth xs (n mod List.length xs)

let rec stream_first_k_such_that p k (Stream t) =
  failwith "stream_first_k_such_that: not implemented"

let funny_number_stream : int stream =
  Stream (fun () -> failwith "funny_number_stream: not implemented")

let foo_then_bar : string stream =
  Stream (fun () -> failwith "foo_then_bar: not implemented")

let stream_pair_all_with_one s =
  failwith "stream_pair_all_with_one: not implemented"

let cycle_lists xs ys = failwith "cycle_lists: not implemented"
let array_assoc key a = failwith "array_assoc: not implemented"
let caching_assoc xs n = failwith "caching_assoc: not implemented"
let tokenize s = failwith "tokenize: not implemented"
let rec interpret stack ts = failwith "interpret: not implemented"
