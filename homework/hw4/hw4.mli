open Hw4types

val sequence : int -> int -> int -> int list
val string_append_map : string list -> string -> string list
val list_nth_mod : 'a list -> int -> 'a
val stream_first_k_such_that : ('a -> bool) -> int -> 'a stream -> 'a list
val funny_number_stream : int stream
val foo_then_bar : string stream
val stream_pair_all_with_one : 'a stream -> (int * 'a) stream
val cycle_lists : 'a list -> 'b list -> ('a * 'b) stream
val array_assoc : 'a -> ('a * 'b) option array -> 'b option
val caching_assoc : ('a * 'b) list -> int -> 'a -> 'b option
val tokenize : string -> token list
val interpret : int list -> token list -> int list
