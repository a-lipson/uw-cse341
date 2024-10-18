(* NOTE: Uncomment the following line if you want to #use this file in utop
 * (optionally, call it from utop directly): *)
(* #mod_use "hw3types.ml";; *)


(* NOTE: to get rid off the red-wiggles in VSCode, first compile the
 * the hw3types module running this 
 * from the command line: 
       ocamlopt hw3types.ml
 *)
open Hw3types

(**** Implement the following functions, remembering the "important note on function bindings" in the assignment write-up ****)

(* #1 *)
let rec search_exn xs n = 
  match xs with
  | [] -> raise NotFound
  | x :: xs' -> if x = n then 0 else 1 + search_exn xs' n

(* #2 *)
let search xs n = 
  try let x = search_exn xs n in Some x
  with NotFound -> None 

(* #3 *)
let rec add n m = 
  match n with 
  | Zero -> m 
  | (Succ n) -> add n (Succ m)

let to_int n =
  let rec aux x acc = 
    match x with 
    | Zero -> acc 
    | Succ x -> aux x (acc+1)
  in aux n 0

let from_int n = 
  if n < 0 then raise NegativeInput else
  let rec aux x acc = 
    match x with 
    | 0 -> acc
    | x -> aux (x-1) (Succ acc)
  in aux n Zero

(* #4 *)
(* assume strings non empty *)
let only_lowercase =
  List.filter (fun s -> Char.lowercase_ascii s.[0] = s.[0])

(* #5 *)
let longest_string1 = 
  List.fold_left (fun s s' -> if String.length s' > String.length s then s' else s) ""

(* #6 *)
let longest_string2 =
  List.fold_left (fun s s' -> if String.length s' >= String.length s then s' else s) ""

(* #7 *)
let longest_string_helper f =
  List.fold_left (fun s s' -> if f s s' then s' else s) ""

let longest_string3 =
  longest_string_helper (fun s1 s2 -> String.length s2 > String.length s1)

let longest_string4 =
  longest_string_helper (fun s1 s2 -> String.length s2 >= String.length s1)

(* #8 *)
let longest_lowercase = longest_string1 % only_lowercase

(* #9 *)
let caps_no_X_string = 
  String.concat "" 
  % String.split_on_char 'X' 
  % String.uppercase_ascii  

(* #10 *)
let rec first_answer f xs = 
  match xs with 
  | [] -> raise NoAnswer
  | x :: xs -> 
    match f x with 
    | Some v -> v 
    | None -> first_answer f xs 

(* #11 *)
let all_answers f xs = 
  List.fold_left
    (fun acc x -> Option.bind acc 
      (fun lst -> Option.map ((@) lst) (f x)))
    (Some []) xs

(* #12 *)
let count_wildcards = g (fun _ -> 1) (fun _ -> 0) 

let count_wild_and_variable_lengths = g (fun _ -> 1) String.length

let count_a_var s = g (fun _ -> 0) (fun v -> if s = v then 1 else 0)

(* #13 *)
let check_pat pat =
  let rec has_no_repeats xs =
    match xs with 
    | [] -> true 
    | x :: xs -> 
        if List.mem x xs then false
        else has_no_repeats xs
  in 
  let rec get_var_names acc p =
    match p with 
    | VariableP x -> x :: acc
    | ConstructorP (_,p) -> get_var_names acc p
    | TupleP ps -> List.fold_left get_var_names acc ps
    | _ -> acc 
  in 
  has_no_repeats (get_var_names [] pat)
  (* let has_repeats xs =  *)
  (*   List.exists (fun x -> List.length (List.filter ((=) x) xs) > 1) xs *)
  (* in  *)


(* #14 *)
let rec matches v pat = 
  match pat with 
  | VariableP x -> 
  | TupleP ps -> all_answers 
  | WildcardP -> Some []

(* #15 *)
let first_match v pats = 
   "Need to implement first_match"

(* optional challenge problem  *)

let typecheck_patterns cons pats = 
   "Need to implement typecheck_patterns"
