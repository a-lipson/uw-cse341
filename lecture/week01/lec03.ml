(* Pairs *)
let my_pair : int * int = (3, 4)
(* my_pair evaluates to (3, 4) *)

let my_pair2 : int * string = (1 + 7 * 3, "hi" ^ " CSE 341")
(* my_pair2 evaluates to (22, "hi CSE 341") *)

(* Nesting *)
let my_nested_pair : (int * int) * (string * (bool * bool)) = ((1,2), ("hi", (false, true)))


(* Function that sums the two integers in an int * int pair *)
let sum (pair : (int * int)) = (fst pair) + (snd pair)


(* [(1, true); (2, false); (3, true && false)] *)

let my_list = [1; 2; 3] ;;
let my_list2 = [4; 5; 6] ;;

let x = my_list @ my_list2 ;;
(* x evaluates to [1;2;3;4;5;6] *)


let rec sum_list (l : int list) =
  if l = []
    then 0
else (List.hd l) + (sum_list (List.tl l))
