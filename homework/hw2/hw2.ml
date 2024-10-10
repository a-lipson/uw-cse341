(* CSE 341, HW2 Provided Code *)

(* This is from file json.ml in this directory. json.ml
 * contains the main datatype definition we will use throughout the
 * assignment. You will want to look over this file before starting. *)
include Json

(* These come from the parsed_*_bus.ml.
   Each file binds one variable: small_bus_positions (10 reports),
   medium_bus_positions (100 reports), and complete_bus_positions (~1000 reports),
   respectively with the data that you will need to implement
   your homework.
*)
open Json_structures.Parsed_complete_bus
open Json_structures.Parsed_medium_bus
open Json_structures.Parsed_small_bus

(* provided helper function that deduplicates a list *)
let dedup xs = List.sort_uniq compare xs

(* provided helper function that sorts a given list *)
let sort xs = List.sort compare xs

(* provided helper function to convert a float to a string *)
(* OCaml's string_of_float is not quite RFC compliant due to its tendency
   to output whole numbers with trailing decimal points without a zero.
   But, printf does the job how we want. *)
let json_string_of_float f =
  Printf.sprintf "%g" f
  
(* 1 *)
let make_silly_json i =
  let rec aux n = 
    if n = 0 then (Array []) else
      (* let Array next = aux (n-1) in <-- with this, get an inexhaustive pattern match warning. *)
      match aux (n-1) with 
      | Array next -> Array (Object 
        [ ("n", Num (float_of_int n))
        ; ("b", True)
        ] :: next)
      | _ -> failwith "Recurive calls should only produce Array type"
  in aux i

(* 2 *)
let rec concat_with (sep, ss) =
  match ss with 
  | [] -> ""
  | [x] -> x
  | x::xs -> x ^ sep ^ concat_with (sep, xs)

(* 3 *)
let quote_string s = "\"" ^ s ^ "\""

(* 4 *)
let rec string_of_json j =
  match j with 
  | Null -> "null"
  | False -> "false"
  | True -> "true"
  | Num x -> json_string_of_float x
  | String s -> quote_string s
  | Array l -> let rec parse_array js = 
    match js with 
    | [] -> []
    | [x] -> [string_of_json x]
    | x::xs -> string_of_json x :: parse_array xs
    in "[" ^ concat_with (", ", parse_array l) ^ "]" 
  | Object o -> let rec parse_object js =
    match js with 
    | [] -> []
    | [(k,v)] -> [quote_string k ^ " : " ^ string_of_json v]
    | (k,v)::xs -> (quote_string k ^ " : " ^ string_of_json v) :: parse_object xs
    in "{" ^ concat_with (", ", parse_object o) ^ "}"

(* 5 *)
let rec take (n,xs) = 
  match n, xs with 
  | 0, _ | _, [] -> []
  | n, x::xs -> x :: take (n-1, xs)

(* 6 *)
(* firsts :: [(a,b)] -> [a] *)
let rec firsts xs = 
  match xs with 
  | [] -> []
  | (a, _) :: xs -> a :: firsts xs

(* 7 *)
(* write your comment here *)
(* 
 * firsts (take (n, xs))
 * take (n, firsts xs)
 * 
 * Both functions are equivalent to one another. 
 * Either the list of int pairs is reduced to an int list with first and then elements are taken,
 * or the elements of the int pair list are taken and then the first reduction is applied.
 * In other words, the composition of the functions is commutative.
 *
 * Since firsts has to match on two values instead of one, it may be faster to take n of length xs pairs first,
 * and then map firsts.
 *)

(* 8 *)
(* assoc a -> [(a, b)] -> Maybe b *)
let rec assoc (k, xs) =
  match xs with 
  | [] -> None 
  | (k', v) :: xs -> if k = k' then Some v else assoc (k, xs)

(* 9 *)
let dot (j, f) = 
  match j with 
  | Object o -> assoc (f, o)
  | _ -> None

(* 10 *)
let rec dots (j, fs) =
  match j, fs with 
  | _, [] -> None 
  | Object o, f::[] -> assoc (f, o)
  | Object _, f::fs -> 
    (match dot (j, f) with 
    | Some v -> dots (v, fs)
    | None -> None)
  | _, _ -> None


(* 11 *)
let one_fields j =
  let rec aux js acc =
    match js with 
    | [] -> acc 
    | (k, Object o) :: js' -> aux o (aux js' (k :: acc)) (* FIXME: nested nonsense *)
    | (k, _) :: js' -> aux js' (k :: acc)
  in match j with 
  | Object o -> aux o []
  | _ -> []

(* 12 *)
let no_repeats xs = 
  List.length (dedup xs) = List.length xs

(* 13 *)
let rec recursive_no_field_repeats j = 
  no_repeats (one_fields j)
(* 14 *)
let count_occurrences xs =
  failwith "Need to implement: count_occurrences"

(* 15 *)
let rec string_values_for_access_path (fs, js) = 
  failwith "Need to implement: string_values_for_access_path"

(* 16 *)
let rec filter_access_path_value (fs, v, js) = 
  failwith "Need to implement: filter_access_path_value"

(* Types for use in problems 17-20. *)
type rect = { min_latitude: float; max_latitude: float;
              min_longitude: float; max_longitude: float }
type point = { latitude: float; longitude: float }

(* 17 *)
let in_rect (r, p) = 
  failwith "Need to implement: in_rect"

(* 18 *)
let point_of_json j = 
  failwith "Need to implement: point_of_json"

(* 19 *)
let rec filter_access_path_in_rect (fs, r, js) = 
  failwith "Need to implement: filter_access_path_in_rect"

(* 20 *)
(* write your comment here *)

(* For this section, we provide the definition of U district and the functions
 * to calculate a histogram. Use these to create the bindings as requested. 
 * But notice our implementation of histogram uses *your* definition of count_occurrences
 *)
 (* We provide this code commented out because it uses some of your functions 
    that you haven't implemented yet *)

(*

(* The definition of the U district for purposes of this assignment :) *)
let u_district =
  { min_latitude  =  47.648637;
    min_longitude = -122.322099;
    max_latitude  =  47.661176;
    max_longitude = -122.301019
  }

(* Creates a histogram for the given list of strings. 
 * Returns a tuple in which the first element is
 * a string, and the second is the number of times that string
 * is found. *)
let histogram xs = 
  let sorted_xs = List.sort (fun a b -> compare a b) xs in
  let counts = count_occurrences sorted_xs in
  let compare_counts (s1, n1) (s2, n2) =
    if n1 = n2 then compare s1 s2 else compare n1 n2
  in
  List.rev (List.sort compare_counts counts)

let histogram_for_access_path (fs, js) = 
  histogram (string_values_for_access_path (fs,js))

(* notice we use *your* definition of dot *)
let complete_bus_positions_list =
  match (dot (complete_bus_positions, "entity")) with
  | Some (Array xs) -> xs
  | _ -> failwith "complete_bus_positions_list"

*)
exception Unimplemented
let route_histogram     = Unimplemented
let top_three_routes    = Unimplemented
let buses_in_ud         = Unimplemented
let ud_route_histogram  = Unimplemented
let top_three_ud_routes = Unimplemented
let all_fourty_fours    = Unimplemented
