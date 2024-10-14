(* CSE 341, Homework 2 Tests *)
open Hw2
open Hw2challenge
open Json

(* This file provides a list of basic tests for your homework.
 * You will surely want to add more! These tests do not guarantee that your code
 * is correct or will pass autograder tests. 
 * Notice that currently calling any of the functions on hw2.ml will fail,
 * as such, all test functions are commented by default. As you
 * work on implementing the homework functions:
 *   1. Implement the changes on hw2.ml
 *   2. Run `dune build` to make sure everything compiles
 *   3. Uncomment the corresponding test
 *   4. Add more test scenarios
 *   5. Run `dune test` to build and run your tests.
 * If working correctly, `dune test` will complete with no error messages
 *)

(* We leave the first test uncommented to get you started. Until make_silly_json
 * gets implemented, calling `dune test` or `#use "hw2test.ml"` from dune utop 
 * will print a Failure message: *)
let%test "make_silly_json_some" = make_silly_json 2 = Array
    [Object [("n", Num 2.); ("b", True)]; 
     Object [("n", Num 1.); ("b", True)]]

let%test "make_silly_json_empty" = make_silly_json 0 = Array []

let%test "concat_with_test_empty" = concat_with (";", []) = ""
let%test "concat_with_test_single" = concat_with (";", ["1"]) = "1"
let%test "concat_with_test_some" = concat_with (";", ["1"; "2"]) = "1;2"

let%test "quote_string_test" = quote_string "hello" = "\"hello\""

let%test "string_of_json_simple" = string_of_json Null = "null"
let%test "string_of_json_some" = string_of_json json_obj = "{\"foo\" : 3.14159, \"bar\" : [1, \"world\", null], \"ok\" : true}"
let%test "string_of_json_nested" = string_of_json (Object [("a", Object [("b", True)])]) = "{\"a\" : {\"b\" : true}}"

let%test "take_test_none" = take (0, []) = []
let%test "take_test_some" = take (2, [4; 5; 6; 7]) = [4; 5]

let%test "first_test_none" = firsts [] = []
let%test "first_test_some" = firsts [(1,2); (3,4)] = [1; 3]

(** don't forget to write a comment for problem 7 **)

let%test "assoc_test_empty" = assoc ("foo", []) = None
let%test "assoc_test_present" = assoc ("foo", [("bar",17);("foo",19)]) = Some 19
let%test "assoc_test_missing" = assoc ("foo", [("bar",17);("baz",19)]) = None

let%test "dot_test_present" = dot (json_obj, "ok") = Some True
let%test "dot_test_missing" = dot (json_obj, "baz") = None
let%test "dot_test_not_object" = dot (json_pi, "a") = None

let%test "dots_test_present" = dots (Object [("f", Object [("g", String "gotcha")])], ["f"; "g"]) = Some (String "gotcha")
let%test "dots_test_missing" = dots (Object [("f", Object [("g", String "gotcha")])], ["f"; "h"]) = None

let%test "one_fields_top" = one_fields json_obj = ["foo";"bar";"ok"]
let%test "one_fields_nested" = one_fields (Object [("a", Object [("b", True)])]) = ["a"]
(* let%test "one_fields_nested" = one_fields (Object [("a", Object [("b", True)])]) = List.rev ["a";"b"] *)

let%test "no_repeats_test_repeats" = not (no_repeats ["foo";"bar";"foo"])
let%test "no_repeats_test_clear" = (no_repeats ["foo";"bar";"baz"])

let nest = Array [Object [];
                  Object[("a",True);
                         ("b",Object[("foo",True);
                                     ("foo",True)]);
                         ("c",True)];
                  Object []]


let distinct_duplicates = Array [Object [];
                  Object[("a",True);
                         ("b",Object[("foo",True)]);
                         ("c",Object[("foo",True)])];
                  Object []]


let%test "recursive_no_field_repeats_test_nested" = not (recursive_no_field_repeats nest)
let%test "recursive_no_field_repeats_test_distinct" = recursive_no_field_repeats distinct_duplicates
let%test "recursive_no_field_repeats_test_clean" = recursive_no_field_repeats json_obj

(* Any order is allowed by the specification, so it's ok to fail this test because of a different order. 
   You can edit this test to match your implementation's order. *)
let%test "count_occurrences_test_some" = count_occurrences (["a"; "a"; "b"]) = [("b",1);("a",2)]
let%test "count_occurrences_test_empty" = count_occurrences [] = []

let%test "string_values_for_access_path_test_some" = string_values_for_access_path (
  ["x"; "y"], [Object [("a", True);("x", Object [("y", String "foo")])]; Object [("x", Object [("y", String "bar")]); ("b", True)]])
            = ["foo";"bar"]

let%test "filter_access_path_value_test_some" = filter_access_path_value (["x"; "y"], "foo",
                                           [Object [("x", Object [("y", String "foo")]); ("z", String "bar")];
                                            Object [("x", Object [("y", String "foo")]); ("z", String "baz")];
                                            Object [("x", String "a")];
                                            Object []])
             = 
             [Object [("x", Object [("y", String "foo")]); ("z", String "bar")];
              Object [("x", Object [("y", String "foo")]); ("z", String "baz")]]

let pgascse =
  { latitude = 47.653221
  ; longitude = -122.305708 }

let u_district =
  { min_latitude  =  47.648637
  ; min_longitude = -122.322099
  ; max_latitude  =  47.661176
  ; max_longitude = -122.301019 }

let%test "in_rect_test_in" = in_rect (u_district, pgascse)
let%test "in_rect_test_out" = not (in_rect (u_district, { latitude = 50. ; longitude = 120. }))

let json_pgascse = Object [("latitude", Num 47.653221); ("longitude", Num (-122.305708))]

let%test "point_of_json_test_some" = point_of_json json_pgascse = Some pgascse
let%test "point_of_json_test_part" = point_of_json (Object [("latitude", Num 0.)]) = None
let%test "point_of_json_test_bad_type" = point_of_json (Object [("latitude", True);("longitude", False)]) = None
let%test "point_of_json_test_none" = point_of_json json_obj = None

let%test "filter_access_path_in_rect_test" = filter_access_path_in_rect (["x"; "y"], u_district, [Object [("x", Object [("y", json_pgascse)])]; Object []])
             = [Object [("x", Object [("y", json_pgascse)])]]

(* Challenge problems *)

let%test "consume_string_literal_test" = consume_string_literal (char_list_of_string "\"foo\" : true") = ("foo", [' '; ':'; ' '; 't'; 'r'; 'u'; 'e'])

let%test "consume_keyword_test" = consume_keyword (char_list_of_string "false foo") = (FalseTok, [' '; 'f'; 'o'; 'o'])

let%test "tokenize_char_list_test" = tokenize_char_list (char_list_of_string "{ \"foo\" : 3.14, \"bar\" : [true, false] }")
             = [LBrace; StringLit "foo"; Colon; NumLit "3.14"; Comma; StringLit "bar";
                Colon; LBracket; TrueTok; Comma; FalseTok; RBracket; RBrace]

(* let%test "testC5" = parse_string [StringLit "foo"; FalseTok] = ("foo", [FalseTok]) *)

(* let%test "testC6" = expect (Colon, [Colon; FalseTok]) = [FalseTok] *)

(* let%test "testC10" = parse_json (tokenize "{ \"foo\" : null, \"bar\" : [true, false] }")
              = (Object [("foo", Null); ("bar", Array [True; False])], []) *)

