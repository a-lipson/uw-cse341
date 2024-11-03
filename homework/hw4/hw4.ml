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

(* assume k non-negative *)
let rec stream_first_k_such_that p k (Stream t) =
  if k = 0 then [] else
  let h, r = t () in 
  if p h then h :: stream_first_k_such_that p (k-1) r 
  else stream_first_k_such_that p k r

let rec stream_map f (Stream s) = 
  let (x, s') = s () in 
  Stream (fun () -> (f x, stream_map f s'))

(* helper stream *)
let nats = 
  let rec go n = (n, Stream (fun () -> go (n + 1)))
  in Stream (fun () -> go 0)


let funny_number_stream : int stream =
  let f x = if x mod 6 = 0 then -x else x
  in stream_map f nats
  (* let f x = if x mod 6 = 0 then -x else x in  *)
  (* let rec next n = (f n, Stream (fun () -> next (n + 1))) *)
  (* in Stream (fun () -> next 1) *)

let foo_then_bar : string stream =
  let f x = if x mod 2 = 0 then "foo" else "bar" 
  in stream_map f nats
  (* let rec next b = ((if b then "foo" else "bar"),  *)
  (*                   Stream (fun () -> next (not b))) *)
  (* in Stream (fun () -> next true) *)

let stream_pair_all_with_one s = stream_map (fun x -> (1, x)) s

(* assume both lists non-empty *)
let cycle_lists xs ys = 
  let f x = (list_nth_mod xs x, list_nth_mod ys x)
  in stream_map f nats
  (* let rec next n = ((list_nth_mod xs n, list_nth_mod ys n),  *)
  (*                   Stream (fun () -> next (n+1))) *)
  (* in Stream (fun () -> next 0)  *)

let array_assoc key a = failwith "array_assoc: not implemented"

let caching_assoc xs n = failwith "caching_assoc: not implemented"

let tokenize s = failwith "tokenize: not implemented"

let rec interpret stack ts = failwith "interpret: not implemented"
