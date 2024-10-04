(*
  The following are sample exam questions.

  To answer them, assign to the corresponding binding the 
  value representing the right option
*)


(*
Question 1:

Consider the following OCaml code:
let rec mystery (lst: int list) : int = 
  if lst = [] then
    1
  else
    (List.hd lst) * (mystery (List.tl lst))

let result = mystery [2; 3; 4]

What will happen when this code is executed?
A) result will be 9
B) result will be 24
C) result will be 20
D) This code will result in an error
E) The code will result in an infinite loop
F) None of the above
*)
let answer_1 = ""


(*
Question 2:

You’re testing a function that navigates through a deeply nested, balanced 
structure of pairs. 
The function takes a string argument where each character is either 
'l' (for left) or 'r' (for right), determining which element of the nested 
pairs to select. Paths like "rrl" direct the function to progressively select 
elements at different levels of nesting.

Here’s the OCaml code:

type i8t = ((int * int) * (int * int)) * ((int * int) * (int * int))

let baz ((p: i8t), (path: string)) : int = 
  if path = "lll" then fst (fst (fst p)) else
  if path = "llr" then snd (fst (fst p)) else
  if path = "lrl" then fst (snd (fst p)) else
  if path = "lrr" then snd (snd (fst p)) else
  if path = "rll" then fst (fst (snd p)) else
  if path = "rlr" then snd (fst (snd p)) else
  if path = "rrl" then fst (snd (snd p)) else
  if path = "rrr" then snd (snd (snd p)) else
  -1  (* gross *)

let p = (((1, 2), (3, 4)), ((5, 6), (7, 8)))

let result = baz (p, "lrl")


What will happen when this code is executed?
A) result will be -1
B) result will be 1
C) result will be 3
D) result will be 4
E) result will be 5
F) result will be 7
G) None of the above
*)
let answer_2 = ""


(*
Question 3:

A new student wants to extend the code from the previous problem to work with 
more paths. They’ve  added the code below under the comment (* NEW CODE *), 
but something is wrong.

type i8t = ((int * int) * (int * int)) * ((int * int) * (int * int))

let baz ((p: i8t), (path: string)) : int = 

  (* NEW CODE *)
  if path = “”    then p else
  if path = “l”   then fst p else
  if path = “r”   then snd p else
  if path = “ll”  then fst (fst p) else
  if path = “lr”  then snd (fst p) else
  if path = “rl”  then fst (snd p) else
  if path = “rr”  then snd (snd p) else

  (* original cases we had in previous problem *)
  if path = "lll" then fst (fst (fst p)) else
  if path = "llr" then snd (fst (fst p)) else
  if path = "lrl" then fst (snd (fst p)) else
  if path = "lrr" then snd (snd (fst p)) else
  if path = "rll" then fst (fst (snd p)) else
  if path = "rlr" then snd (fst (snd p)) else
  if path = "rrl" then fst (snd (snd p)) else
  if path = "rrr" then snd (snd (snd p)) else
  -1  (* gross *)


Which of the following is something incorrect about their design?
A) The check for "l" and "r" should be reversed since fst and snd are swapped.
B) The ”” string comparison does not handle all possible paths, introducing a type mismatch.
C) None of the new cases return an int, so the code cannot type check.
D) The code is missing a base case, leading to an infinite loop.
E) The types of the tuples are inconsistent, leading to a pattern matching error.
*)
let answer_3 = ""

