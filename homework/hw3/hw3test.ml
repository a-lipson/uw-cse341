(* CSE 341, Homework 3 Tests *)
open Hw3

(* Write your tests for Homework 3 in this file.
 * As you work on implementing the homework functions:
 *   1. Implement the changes in hw3.ml
 *   2. Run `dune build` to make sure everything compiles
 *   3. Add test scenarios
 *   4. Run `dune test` to build and run your tests
 * If working correctly, `dune test` will complete with no error messages.
 *)

 let%test "test1" = 
  search_exn [1; 3; 5; 3; 1] 3 
  = 
  1
