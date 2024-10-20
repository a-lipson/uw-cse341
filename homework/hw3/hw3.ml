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
  List.fold_left (fun s s' -> if f (String.length s') (String.length s) then s' else s) ""

let test = longest_string_helper

let longest_string3 =
  longest_string_helper (fun s1 s2 -> s1 > s2)

let longest_string4 =
  longest_string_helper (fun s1 s2 -> s1 >= s2)

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
  let uncurry f (x, y) = f x y in 
  match pat, v with 
  | VariableP s, _ -> Some [(s, v)]
  | ConstantP x, Constant x' -> 
    if x = x' then Some [] else None
  | UnitP, Unit -> Some []
  | WildcardP, _ -> Some []
  | TupleP ps, Tuple vs -> 
    if List.length ps = List.length vs then 
      all_answers (uncurry matches) (List.combine vs ps)
    else None
  | ConstructorP (s,p), Constructor (s',v) -> 
    if s = s' then matches v p else None
  | _, _ -> None

(* #15 *)
let first_match v pats = 
  try let m = first_answer (matches v) pats in Some m
  with NoAnswer -> None


(* optional challenge problem  *)

(* typecheckPatterns :: [(String, String, Typ)] -> [Pattern] -> Maybe Typ*)
(* cons provides variant types and their constructors *)
(* assume cons all have different constructor names (first field) *)
let typecheck_patterns cons pats = 
  let base_t = Some AnythingT in 
  let rec sequence l = 
    if List.for_all Option.is_some l then 
      Some (List.map Option.get l)
    else None
  in
  let rec match_pattern t p =
  match t, p with
  | _, WildcardP -> t
  | _, VariableP _ -> t
  | Some UnitT, UnitP -> base_t
  | Some IntT, ConstantP _ -> base_t
  (* | Some AnythingT, _ -> Some AnythingT *)
  | Some AnythingT, TupleP ps -> 
      List.map (match_pattern base_t) ps
      |> sequence
      |> Option.map ((fun x -> TupleT x) % List.rev)
  | Some TupleT ts, TupleP ps -> 
    if List.length ps = List.length ts then
      List.map2 match_pattern (List.map Option.some ts) ps
      |> sequence 
      |> Option.map (fun x -> TupleT x)
    else None
  | _, ConstructorP (n, p) ->
    List.find_opt (fun (n', _, _) -> n' = n) cons
    |> Fun.flip Option.bind (fun (_, v, t) -> 
      match_pattern (Some t) p 
      |> Option.map (fun _ -> VariantT v))
  | _, _ -> None
  in List.fold_left match_pattern base_t pats

