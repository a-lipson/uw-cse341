open Ast
open Errors

include Interpreter_types

let string_of_dynenv_entry (x, v) = x ^ " -> " ^ string_of_entry v

let rec lookup (dynenv : dynamic_env) name =
  match dynenv with
  | [] -> None
  | (x, value) :: dynenv ->
     if x = name
     then Some value
     else lookup dynenv name

(* expression -> value *)
let rec interpret_expression dynenv expr : expr (* must be value subtype *) =
  let int_binop f le re name = 
      match interpret_expression dynenv le, interpret_expression dynenv re with 
      | Int n1, Int n2 -> f n1 n2
      | Int _, v2 -> raise (RuntimeError (name ^ " applied to non-integer " ^ string_of_expr v2))
      | v1,     _ -> raise (RuntimeError (name ^ " applied to non-integer " ^ string_of_expr v1))
  in match expr with
  | Int  _ -> expr
  | Bool _ -> expr
  | Nil    -> expr (* ? *) 
  | Var x  -> begin 
      match lookup dynenv x with
      | None -> raise (RuntimeError ("Unbound var " ^ x))
      | Some (FunctionEntry (f, _)) -> raise (RuntimeError ("Functions are not values (yet)" ^ show_function_binding f))
      | Some (VariableEntry v) -> v
  end
  | Add (e1, e2) -> Int  (int_binop ( + ) e1 e2 "Add")
  | Sub (e1, e2) -> Int  (int_binop ( - ) e1 e2 "Sub")
  | Mul (e1, e2) -> Int  (int_binop ( * ) e1 e2 "Mul")
  | Eq  (e1, e2) -> Bool (int_binop ( = ) e1 e2 "Eq" )
  | Cons (e1, e2) -> (* eagerly store evalued expressions *) 
      let v1 = interpret_expression dynenv e1 in
      let v2 = interpret_expression dynenv e2 in
      Cons (v1, v2)
  | IsNil e -> begin
      match interpret_expression dynenv e with 
      | Nil -> Bool true 
      | _   -> Bool false 
      end
  | IsCons e -> begin 
      match interpret_expression dynenv e with 
      | Cons _ -> Bool true 
      | _      -> Bool false 
      end
  | Car e -> begin
      match interpret_expression dynenv e with 
      | Cons (l, _) -> l (* interpret_expression dynenv l *)
      | _ -> raise (RuntimeError ("car not applied to cons expression " ^ string_of_expr e))
      end
  | Cdr e -> begin
      match interpret_expression dynenv e with 
      | Cons (_, r) -> r (* interpret_expression dynenv r *)
      | _ -> raise (RuntimeError ("cdr not applied to cons expression " ^ string_of_expr e))
      end
  | If  (pred, thn, els) -> begin
      match interpret_expression dynenv pred with 
      | Bool false -> interpret_expression dynenv els
      | _          -> interpret_expression dynenv thn (* all other values are truthy *)
      end
  | Let (bindings, body) -> (* let bindings cannot refer to other variables actively bound in same let expr *)
      let entries = List.map (fun (name, e) -> 
        (name, VariableEntry (interpret_expression dynenv e))) bindings
      in interpret_expression (entries @ dynenv) body
  | Cond clauses -> 
      let rec eval = function 
      | [] -> raise (RuntimeError "cond expression ran out of clauses")
      | (pred, body) :: cs -> 
          match interpret_expression dynenv pred with
          | Bool false -> eval cs
          | _ -> interpret_expression dynenv body (* all other values are truthy *)
      in eval clauses 
  | Call (name, args) -> begin
      match lookup dynenv name with (* callenv = current dynamic environment *)
      | Some (FunctionEntry (f, defenv)) -> 
          if List.length args <> List.length f.param_names 
          then raise (RuntimeError ("Incorrect number of arguments supplied to function " ^ name));
          let vals = List.map (fun v -> VariableEntry (interpret_expression dynenv v)) args 
          in let extended_environment = (name, FunctionEntry (f, defenv)) :: (List.combine f.param_names vals) @ defenv 
          in interpret_expression extended_environment f.body 
      | _ -> raise (RuntimeError ("Unbound function " ^ name))
      end

(* extend dynamic environment with binding *)
let interpret_binding dynenv b : dynamic_env =
  match b with
  | VarBinding (x, e) ->
      let v = interpret_expression dynenv e in
      Printf.printf "%s = %s\n%!" x (string_of_expr v);
      (x, VariableEntry v) :: dynenv
  | FunctionBinding f -> (f.name, FunctionEntry (f, dynenv)) :: dynenv
  | TopLevelExpr e ->
      let v = interpret_expression dynenv e in
      print_endline (string_of_expr v);
      dynenv
  | TestBinding e -> begin
      match interpret_expression dynenv e with 
      | Bool true -> dynenv
      | _ -> raise (RuntimeError (string_of_expr e ^ " fails"))
      end


(* the semantics of a whole program (sequence of bindings) *)
let interpret_bindings (* dynenv bs *) =
  List.fold_left interpret_binding (* dynenv bs *)

(* starting from dynenv, first interpret the list of bindings in order. then, in
   the resulting dynamic environment, interpret the expression and return its
   value *)
let interpret_expression_after_bindings dynenv bindings (* expr *) =
  interpret_expression (interpret_bindings dynenv bindings) (* expr *)
