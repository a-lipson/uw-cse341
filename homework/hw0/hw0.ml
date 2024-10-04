(* this is a smart way to administer the homework because grading is just a unit test! *)

(* PART 1 *)

(* This function takes a number as an argument and
returns the number that is 10 greater than the input *)
let add10 (x : int) = x + 10 ;;

(* Uncomment the following line and complete the definition *)
let add20 (x : int) = x + 20 ;;

(* PART 2 *)
(*
Problem 1
For each of the following actions, indicate whether it is
permitted by binding the variable to either "allowed" or "not allowed"
*)
(* Talking to a classmate about how to approach a challenging homework problem. *)
let p1a : string = "allowed" ;;

(* Searching on StackOverflow for how to use a match statement in OCaml. *)
let p1b : string = "allowed" ;;

(* Asking a classmate to look at your code to help you debug a problem. *)
let p1c : string = "not allowed" ;;

(* Using ChatGPT to summarize a Wikipedia article about currying. *)
let p1d : string = "allowed" ;;

(* Using ChatGPT to generate code for a homework assignment.
   The code that ChatGPT produces doesn’t work for all inputs, so you modify
   the code to correctly handle an edge case and then submit your assignment. *)
let p1e : string = "not allowed" ;;

(* Searching on Github for solutions to homework assignments from a previous quarter. *)
let p1f : string = "not allowed" ;;

(* Posting a public question on Ed asking for clarification about
   what a homework problem is asking you to do. *)
let p1g : string = "allowed" ;;

(* Posting a private question on Ed containing a snippet of your code
   asking for help on how to handle an edge case. *)
let p1h : string = "allowed" ;;

(*
Problem 2
What do you need to do in order to take advantage of the Resubmission Window?
A. Nothing! Just submit your finished work by Sunday at 5pm.
B. Submit a meaningful attempt by Friday at 5pm.
C. E-mail the course staff letting us know that you will submit by Sunday at 5pm.
D. Submit a blank file to Gradescope by Friday at 5pm.

Indicate your answer by binding `p2` to "A", "B", "C", or "D"
*)
let p2 : string = "B" ;;

(*
Problem 3
Where will announcements and other course-related information be posted?
A. E-mail
B. Canvas
C. Ed

Indicate your answer by binding `p3` to "A", "B", or "C"
*)
let p3 : string = "C" ;;
