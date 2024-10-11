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
  let parse_array_item x = string_of_json x in
  let parse_object_item (k,v) = quote_string k ^ " : " ^ string_of_json v in
  let rec parse f js = (* oh map, my dear friend... where art thou? *)
    match js with 
    | [] -> []
    | v::vs -> f v :: parse f vs
  in 
  match j with 
  | Null -> "null"
  | False -> "false"
  | True -> "true"
  | Num x -> json_string_of_float x
  | String s -> quote_string s
  | Array l -> "[" ^ concat_with (", ", parse parse_array_item l) ^ "]" 
  | Object o -> "{" ^ concat_with (", ", parse parse_object_item o) ^ "}"

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
  match j with 
  | Object o -> firsts o
  | _ -> []

(* let one_fields j = *)
(*   let rec aux js acc = *)
(*     match js with  *)
(*     | [] -> acc  *)
(*     | (k, Object o) :: js' -> aux o (aux js' (k :: acc)) (* FIXME: nested nonsense *) *)
(*     | (k, _) :: js' -> aux js' (k :: acc)  *)
(*   in match j with  *)
(*   | Object o -> aux o [] *)
(*   | _ -> [] *)

(* 12 *)
let no_repeats xs = List.length (dedup xs) = List.length xs

(* 13 *)
let rec recursive_no_field_repeats j = 
  let objects x = x (* id *) in
  let arrays (_,v) = v in
  let rec aux f xs = (* List.fold_left (&&) true (List.map (recursive_no_field_repeats % f) xs) *)
    match xs with
    | [] -> true
    | x :: xs -> recursive_no_field_repeats x && aux f xs
  in
  no_repeats (one_fields j) && 
  match j with 
    | Array l -> aux arrays l
    | Object o -> aux objects o 
    | _ -> true

(* 14 *)
(* assume xs sorted *)
let count_occurrences xs =
  let rec aux s n xs acc =
    match xs with 
    | [] -> (s,n) :: acc
    | x :: xs -> if s = x then 
      aux s (n+1) xs acc else (* cont. to rest while updating current count *)
      aux x 1 xs ((s,n) :: acc) (* push prev count pair to acc then start next count *)
  in match xs with 
  | [] -> [] 
  | x::xs -> aux x 1 xs []

(* 15 *)
let rec string_values_for_access_path (fs, js) = 
    match js with 
    | [] -> []
    | j :: js -> match dots (j, fs) with 
      | Some (String s) -> s :: string_values_for_access_path (fs, js)
      | _ -> string_values_for_access_path (fs, js)

(* 16 *)
let rec filter_access_path_value (fs, v, js) = 
  match js with 
  | [] -> [] 
  | j :: js -> match dots (j, fs) with 
    | Some (String s) -> if s = v then j :: filter_access_path_value (fs, v, js) else []
    | _ -> filter_access_path_value (fs, v, js)

(* Types for use in problems 17-20. *)
type rect = { min_latitude: float; max_latitude: float;
              min_longitude: float; max_longitude: float }
type point = { latitude: float; longitude: float }

(* 17 *)
let in_rect (r, p) = 
  r.min_latitude <= p.latitude && p.latitude <= r.max_latitude &&
  r.min_longitude <= p.longitude && p.longitude <= r.max_longitude

(* 18 *)
let point_of_json j = 
  match dot (j, "latitude") with 
  | Some (Num lat) -> (match dot (j, "longitude") with 
    | Some (Num long) -> Some { latitude = lat ; longitude = long}
    | _ -> None)
  | _ -> None

(* 19 *)
let rec filter_access_path_in_rect (fs, r, js) = 
  match js with 
  | [] -> [] 
  | j :: js -> (match dots (j, fs) with 
    | Some j' -> (match point_of_json j' with 
      | Some p -> if in_rect (r, p) then [j] else []
      | None -> [])
    | None -> []) @ filter_access_path_in_rect (fs, r, js) (* ideally would like to not use concat... *)

(* 20 *)
(* write your comment here *)
(*
 * Both filter_access_path_* functions filter an input json list based on 
 * the presence of a certain value at a given path. 
 * We can proceed to abstract this behavior by implementing the following 
 * function filter_access_path which uses a function f which acts on the value 
 * at the given path to determine whether the origin json in the json list input 
 * should be kept. 
 * 

 let rec filter_access_path (fs, f, js) = 
  match js with 
  | [] -> [] 
  | j :: js -> (match dots (j, fs) with 
    | Some x -> if f x then j :: filter_access_path (fs, f, js) else []
    | None -> filter_access_path (fs, f, js)

 * 
 * i am merely somewhat/10 annoyed, i suppose, as we cannot do the following: 

 let filter_access_path fs f = List.filter (fun j -> Option.exists f (dots (j, fs)))

 * which allows us to both abstract the logic and produce a one-line readable function!
 *)
    
(* For this section, we provide the definition of U district and the functions
 * to calculate a histogram. Use these to create the bindings as requested. 
 * But notice our implementation of histogram uses *your* definition of count_occurrences
 *)
 (* We provide this code commented out because it uses some of your functions 
    that you haven't implemented yet *)

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

exception Unimplemented

let route_histogram     = Unimplemented
let top_three_routes    = Unimplemented
let buses_in_ud         = Unimplemented
let ud_route_histogram  = Unimplemented
let top_three_ud_routes = Unimplemented
let all_fourty_fours    = Unimplemented
