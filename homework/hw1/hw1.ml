(* CSE 341, Homework 1, Provided Code *)

(* use these functions to extract parts of a date *)
let fst3 (x,_,_) = x (* gets the first element of a triple *)
let snd3 (_,x,_) = x (* gets the second element of a triple *)
let thd3 (_,_,x) = x (* gets the third element of a triple *)

(* date 'type' *) 
(* day int , month int , year int *)

(* 1 *)
(* is date1 older than date2 *)
let is_older ((date1 : int * int * int), (date2 : int * int * int)): bool =
  let d1, m1, y1 = fst3 date1, snd3 date1, thd3 date1 in
  let d2, m2, y2 = fst3 date2, snd3 date2, thd3 date2 in
  (y1 < y2) || (y1 = y2 && m1 < m2) || (y1 = y2 && m1 = m2 && d1 < d2)

let test_years_1_is_older = is_older ((1,1,2001), (1,1,2000)) = false
let test_years_2_is_older = is_older ((1,1,2000), (1,1,2001)) = true
let test_months_1_is_older = is_older ((1,2,2000), (1,1,2000)) = false
let test_months_2_is_older = is_older ((1,1,2000), (1,2,2000)) = true
let test_days_1_is_older = is_older ((2,1,2000), (1,1,2000)) = false
let test_days_2_is_older = is_older ((1,1,2000), (2,1,2000)) = true

(* 2 *)
(* how many dates are in given month *)
let rec number_in_month ((dates : (int * int * int) list), (month : int)) : int =
  (* if only we could fold... *)
  if dates = [] then 0 else
    let is_month = if snd3 (List.hd dates) = month then 1 else 0 in
    is_month + number_in_month (List.tl dates, month)

(* where's Haskell's quickTest when we need it... *)
let test_empty_number_in_month = number_in_month ([],1) = 0
let test_none_number_in_month = number_in_month ([(1,2,2000);(2,2,2000)],1) = 0
let test_some_number_in_month = number_in_month ([(1,1,2000);(1,2,2000)],1) = 1
let test_multiple_number_in_month = number_in_month ([(1,1,2000);(2,1,2000)],1) = 2

(* 3 *)
(* how many dates are in the given months, assume no duplicate months *)
let rec number_in_months ((dates: (int * int * int) list), (months: int list)) : int = 
  if dates = [] || months = [] then 0 else 
    number_in_month (dates, List.hd months) + number_in_months (dates, List.tl months)

let test_empty_number_in_months = number_in_months ([],[1]) = 0
let test_none_number_in_months = number_in_months ([(1,2,2000);(2,2,2000)],[1;3]) = 0
let test_some_number_in_months = number_in_months ([(1,1,2000);(1,2,2000)],[1;3]) = 1
let test_multiple_number_in_months = number_in_months ([(1,1,2000);(2,1,2000);(1,2,2000)],[1;2;3]) = 3

(* 4 *)
(* which dates are in the given month *)
let rec dates_in_month ((dates : (int * int * int) list), (month : int)) : (int * int * int) list =
  if dates = [] then [] else
    let date = List.hd dates in 
    let rest = dates_in_month (List.tl dates, month) in
    if snd3 date = month then date :: rest else rest

let test_empty_dates_in_month = dates_in_month ([],1) = []
let test_none_dates_in_month = dates_in_month ([(1,2,2000);(2,2,2000)],1) = []
let test_some_dates_in_month = dates_in_month ([(1,1,2000);(1,2,2000)],1) = [(1,1,2000)]
let test_multiple_dates_in_month = dates_in_month ([(1,1,2000);(2,1,2000)],1) = [(1,1,2000);(2,1,2000)]

(* 5 *)
(* which dates are in the given months, assume no duplicate months *)
let rec dates_in_months ((dates: (int * int * int) list), (months: int list)) : (int * int * int) list = 
  if dates = [] || months = [] then [] else 
    dates_in_month (dates, List.hd months) @ dates_in_months (dates, List.tl months)

let test_empty_dates_in_months = dates_in_months ([],[1]) = []
let test_none_dates_in_months = dates_in_months ([(1,2,2000);(2,2,2000)],[1;3]) = []
let test_some_dates_in_months = dates_in_months ([(1,2,2000);(1,1,2000);(2,2,2000)],[1;3]) = [(1,1,2000)]
let test_multiple_dates_in_months = dates_in_months ([(1,1,2000);(2,1,2000);(1,2,2000)],[1;2;3]) = [(1,1,2000);(2,1,2000);(1,2,2000)]

(* 6 *)
let rec get_nth ((s: string list), (n: int)) : string = 
  if n = 1 then List.hd s else 
    get_nth (List.tl s, n - 1)

(* let test_empty_get_nth = get_nth [] = *)
let test_singleton_get_nth = get_nth (["a"],1) = "a"
let test_first_get_nth = get_nth (["a";"b"],1) = "a"
let test_second_get_nth = get_nth (["a";"b";"c"],2) = "b"

(* 7 *)
let string_of_date (date: (int * int * int)) : string = 
  let months = ["January";"February";"March";"April";"May";"June";"July";"August";"September";"October";"November";"December"] in
  (get_nth (months, (snd3 date))) ^ "-" ^ (string_of_int (fst3 date)) ^ "-" ^ (string_of_int (thd3 date))

let test_one_string_of_date = string_of_date (1,1,2000) = "January-1-2000"
let test_two_string_of_date = string_of_date (31,12,1999) = "December-31-1999"

(* 8 *)
(* how many elements starting can sum to less than the sum param, 
 * assume all positive, assume sum of list is greater than sum param *)
let number_before_reaching_sum ((sum: int), (lst: int list)) : int = 
  let rec aux max curr n lst = 
    if lst = [] then n-1 else
      let next = curr + List.hd lst in 
      if max <= next then n else
        aux max next (n + 1) (List.tl lst)
  in aux sum 0 0 lst


let test_none_number_before_reaching_sum = number_before_reaching_sum (0,[0;1;2;3]) = 0
let test_one_number_before_reaching_sum = number_before_reaching_sum (1,[0;1;2;3]) = 1
let test_some_number_before_reaching_sum = number_before_reaching_sum (4,[1;2;3]) = 2
let test_many_number_before_reaching_sum = number_before_reaching_sum (4,[0;1;2;3]) = 3

(* 9 *)
let what_month (day: int) : int =
  let month_ends = [31;28;31;30;31;30;31;31;30;31;30;31] in
  number_before_reaching_sum (day, month_ends) + 1

let test_one_what_month = what_month 1 = 1 
let test_two_what_month = what_month 35 = 2
let test_three_what_month = what_month 180 = 6
let test_four_what_month = what_month 365 = 12

(* 10 *)
(* what months have each of the days in between day1 and day2 *)
let rec month_range ((day1: int), (day2: int)) : int list = 
  if day1 > day2 then [] else 
    what_month day1 :: month_range (day1 + 1, day2)

let test_empty_month_range = month_range (2,1) = []
let test_single_month_range = month_range (1,1) = [1]
let test_multiple_1_month_range = month_range (1,3) = [1;1;1]
let test_multiple_2_month_range = month_range (180,184) = [6;6;7;7;7]

(* 11 *)
(* partial sums of xs *)
let cumulative_sum (xs: int list) : int list = 
  (* Haskell folding puts accumulator after list, also, scanl1*)
  let rec aux xs acc = 
    if xs = [] then [] else
      let next = acc + List.hd xs in 
      next :: aux (List.tl xs) next
  in aux xs 0

let test_empty_cumulative_sum = cumulative_sum [] = []
let test_singleton_cumulative_sum = cumulative_sum [1] = [1]
let test_multiple_cumulative_sum = cumulative_sum [12; 27; 13] = [12; 39; 52]
