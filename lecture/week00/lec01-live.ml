(* A program is a sequence of bindings. *)

(* variable binding *)
let x = 3

(* Static Semantics *)
(* Static environment: {x : int} *)

(* Dynamic Semantics*)
(* Dynamic environment: {x: 3} *)

let y = 6

(* Static environment : {y: int; x: int} *)
(* Dynamic environment: {y: 5; x: 3} *)
