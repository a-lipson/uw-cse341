open Ast
open Errors

type dynamic_env = (string * expr) list
let string_of_dynenv_entry (x, v) = x ^ " -> " ^ string_of_expr v

let rec lookup dynenv name =
  match dynenv with
  | [] -> None
  | (x, value) :: dynenv ->
     if x = name
     then Some value
     else lookup dynenv name

(* expression -> value *)
let rec interpret_expression dynenv e =
  let int_binop f le re name = 
    match interpret_expression dynenv le, interpret_expression dynenv re with 
    | Int n1, Int n2 -> f n1 n2
    | Int _, v2 -> raise (RuntimeError (name ^ " applied to non-integer " ^ string_of_expr v2))
    | v1,     _ -> raise (RuntimeError (name ^ " applied to non-integer " ^ string_of_expr v1))
  in match e with
  | Int  _ -> e
  | Bool _ -> e
  | Nil    -> e (* ? *) 
  | Var x  -> begin 
      match lookup dynenv x with
      | None -> raise (RuntimeError ("Unbound var " ^ x))
      | Some value -> value
      end
  | Add (e1, e2) -> Int  (int_binop ( + ) e1 e2 "Add")
  | Sub (e1, e2) -> Int  (int_binop ( - ) e1 e2 "Sub")
  | Mul (e1, e2) -> Int  (int_binop ( * ) e1 e2 "Mul")
  | Eq  (e1, e2) -> Bool (int_binop ( = ) e1 e2 "Eq" )
  | If  (pred, thn, els) -> begin
      match interpret_expression dynenv pred with 
      | Bool false -> interpret_expression dynenv els
      | _          -> interpret_expression dynenv thn (* all other values are truege,*)
      end
  | Let (x, e1, e2) -> 
      let v = interpret_expression dynenv e1 in
      interpret_expression ((x, v) :: dynenv) e2
  | _ -> (failwith ("interpret_expression: not implemented " ^ string_of_expr e))

(* extend dynamic environment with binding *)
let interpret_binding dynenv b =
  match b with
  | VarBinding (x, e) ->
      let v = interpret_expression dynenv e in
      Printf.printf "%s = %s\n%!" x (string_of_expr v);
      (x, v) :: dynenv
  | TopLevelExpr e ->
      let v = interpret_expression dynenv e in
      print_endline (string_of_expr v);
      dynenv
  | TestBinding e -> 
      match interpret_expression dynenv e with 
      | Bool true -> dynenv
      | _ -> RuntimeError ("Test" ^ string_of_expr e ^ "fails") |> raise 

(* the semantics of a whole program (sequence of bindings) *)
let interpret_bindings (* dynenv bs *) =
  List.fold_left interpret_binding (* dynenv bs *)


(* starting from dynenv, first interpret the list of bindings in order. then, in
   the resulting dynamic environment, interpret the expression and return its
   value *)
let interpret_expression_after_bindings dynenv bindings (* expr *) =
  interpret_expression (interpret_bindings dynenv bindings) (* expr *)
