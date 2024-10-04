(* Edit this binding's value so that it is about the current air temperature in Fahrenheit. *)
let temp_f: float = 55.0

(* Write a binding for a variable temp_c that is the conversion of temp_f to Celsius. *)
(* YOUR BINDING HERE *)

(* What edits would you need to make to change the computations above to be over ints instead of floats?
   (Don't actually make the edits, just say what you would do at a high level.) *)
(* YOUR ANSWER HERE *)

(* Write a function c_of_f of type float -> float that converts its argument from Fahrenheit to Celsius.

   Hint: remind yourself how to define a function from lecture. *)
(* YOUR BINDING HERE *)

(* Write a test for your c_of_f implementation. (Remember that for HW1, a test is just a variable binding
   that calls the function and checks its output.) *)
(* YOUR TEST HERE *)

(* Anjali feels cold when it is below 20 degrees Celsius. Write a function is_anjali_cold that takes a
   *Fahrenheit* temperature as an argument (of type float) and returns a bool indicating whether
   Anjali is cold. *)
(* YOUR BINDING HERE *)

(* Write two tests for is_anjali_cold that cover both the case where Anjali is cold and the case where
   she is not cold. *)
(* YOUR FIRST TEST HERE *)
(* YOUR SECOND TEST HERE *)

(* Write a binding for a variable called msg1 of type string that says "Anjali is cold" if Anjali is
   currently cold and "Anjali is not cold" otherwise. Call is_anjali_cold on the current temperature,
   use that to compute the right string. *)
(* YOUR BINDING HERE *)

(* Write a function sum_of_first_n_ints of type int -> int that takes an argument n and returns the
  sum of the first n numbers. Assume that n is non-negative. (Use recursion because we don't have loops!) *)
(* YOUR BINDING HERE *)

(* Write three tests for sum_of_first_n_ints that cover the "0", "1", and "many" cases of the recursion. *)
(* YOUR FIRST TEST HERE *)
(* YOUR SECOND TEST HERE *)
(* YOUR THIRD TEST HERE *)
