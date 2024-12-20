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
  if k = 0 then [] 
  else let h, r = t () in 
    if p h then h :: stream_first_k_such_that p (k-1) r 
    else stream_first_k_such_that p k r

let rec stream_map f (Stream s) = 
  let (x, s') = s () in 
  Stream (fun () -> (f x, stream_map f s'))

(* helper streams *)
let nats = (* natural numbers *)
  let rec aux n = (n, Stream (fun () -> aux (n + 1)))
  in Stream (fun () -> aux 0)

(* positive naturals / whole numbers *)
let pos_nats = snd (match nats with Stream s -> s ())

let funny_number_stream : int stream =
  stream_map (fun x -> if x mod 6 = 0 then -x else x) pos_nats
  (* let f x = if x mod 6 = 0 then -x else x in *)
  (* let rec next n = (f n, Stream (fun () -> next (n + 1))) *)
  (* in Stream (fun () -> next 1) *)

let foo_then_bar : string stream =
  stream_map (fun x -> if x mod 2 = 0 then "foo" else "bar" ) nats
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


(* array_assoc : 'a -> ('a * 'b) option array -> 'b option *)
let array_assoc key a =
  let len = Array.length a in 
  let rec aux i = 
    if i >= len then None 
    else match a.(i) with
    | Some (k, v) -> 
        if k = key then Some v 
        else aux (i + 1)
    | None -> aux (i + 1)
  in aux 0

(* assume n positive *)
(* caching_assoc : ('a * 'b) list -> int -> 'a -> 'b option *)
let caching_assoc xs n = 
  fun k ->  
    let i = ref 0 in
    let c = Array.make n None in 
      match array_assoc k c with 
      | Some v -> Some v 
      | None -> 
          match List.assoc_opt k xs with 
          | Some v -> 
              c.(!i) <- Some (k, v);
              i := (!i + 1) mod n;
              Some v
          | None -> None


let tokenize s = 
  let rec aux s ts = match s with 
  | [] ->  ts 
  | "+" :: s' -> aux s' (Plus :: ts)
  | "-" :: s' -> aux s' (Minus :: ts)
  | "*" :: s' -> aux s' (Mul :: ts)
  | "." :: s' -> aux s' (Dot :: ts)        
  | x   :: s' -> 
      let i = match int_of_string_opt x with 
      | Some i -> i  
      | None -> raise (TrefoilError "syntax error")
      in aux s' ((Literal i) :: ts)
  in aux (String.split_on_char ' ' s) [] |> List.rev

(* interpret : int list -> token list -> int list *)
let rec interpret stack ts =
  let op f = match stack with 
  | x :: y :: s -> f y x :: s 
  | _ -> raise (TrefoilError "insufficient stack allocated")
  in match ts with
  | [] -> stack
  | Literal n :: ts -> interpret (n :: stack) ts 
  | Plus      :: ts -> interpret (op (+)    ) ts
  | Minus     :: ts -> interpret (op (-)    ) ts
  | Mul       :: ts -> interpret (op ( * )  ) ts (* when your comment syntax is goofy *)
  | Dot       :: ts -> 
      match stack with 
      | x :: s -> 
          x |> string_of_int |> print_endline; 
          interpret s ts
      | _ -> raise (TrefoilError "empty stack")
