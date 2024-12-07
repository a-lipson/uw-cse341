open Ast
open Errors

let string_of_dynenv_entry (x, v) = x ^ " -> " ^ string_of_expr v

let rec lookup (dynenv : dynamic_env) name =
  match dynenv with
  | [] -> None
  | (x, value) :: dynenv ->
      if x = name
      then Some value
      else lookup dynenv name

let rec interpret_pattern pattern value : dynamic_env option =
  match pattern, value with
  | WildcardPattern, _ -> Some []
  | ConsPattern (p1, p2), Cons (v1, v2) -> begin
      match interpret_pattern p1 v1, interpret_pattern p2 v2 with
      | Some l1, Some l2 -> Some (l1 @ l2)
      | _ -> None
      end
  (* TODO: add cases for other kinds of patterns here *)
  | NilPattern, _ 
  | VarPattern _, _ 
  | IntPattern _, _
  | BoolPattern _, _ 
  | SymbolPattern _, _ 
  | StructPattern _, _

  | _ -> None


let rec equals e1 e2 = 
  match e1, e2 with
  | Closure _, _ 
  | _, Closure _ -> raise (RuntimeError "Cannot compare closures for equality")
  | Int i1, Int i2 -> i1 = i2 
  | Bool b1, Bool b2 -> b1 = b2 
  | Nil, Nil -> true 
  | Symbol s1, Symbol s2 -> String.equal s1 s2
  | Cons (v1a, v1b), Cons (v2a, v2b) -> equals v1a v2a && equals v1b v2b
  | StructConstructor (s1, vs1), StructConstructor (s2, vs2) -> 
      String.equal s1 s2 && 
      List.length vs1 = List.length vs2 && 
      List.fold_left (&&) true (List.map2 equals vs1 vs2)
  | _, _ -> false


let rec interpret_expression dynenv expr : (* value *) expr =
  let int_binop f le re name = 
      match interpret_expression dynenv le, interpret_expression dynenv re with 
      | Int n1, Int n2 -> f n1 n2
      | Int _, v2 -> raise (RuntimeError (name ^ " applied to non-integer " ^ string_of_expr v2))
      | v1,     _ -> raise (RuntimeError (name ^ " applied to non-integer " ^ string_of_expr v1))

  in match expr with
  | Nil       -> expr
  | Int  _    -> expr
  | Bool _    -> expr
  | Symbol _  -> expr
  | Closure _ -> expr

  | Var x     -> begin 
      match lookup dynenv x with
      | None ->   
          (* print_endline ("current env " ^ string_of_dynamic_env dynenv); *)
          raise (RuntimeError ("Unbound var " ^ x))
      | Some e -> interpret_expression dynenv e 
      end

  | Add (e1, e2) -> Int  (int_binop ( + ) e1 e2 "Add")
  | Sub (e1, e2) -> Int  (int_binop ( - ) e1 e2 "Sub")
  | Mul (e1, e2) -> Int  (int_binop ( * ) e1 e2 "Mul")

  (* TODO: update eq to perform structural equality check (cannot contain closures!) *)
  | Eq  (e1, e2) -> 
      let v1 = interpret_expression dynenv e1 in 
      let v2 = interpret_expression dynenv e2 in 
      Bool (equals v1 v2)

  | IsNil e -> begin
      match interpret_expression dynenv e with 
      | Nil -> Bool true 
      | _   -> Bool false 
      end

  | Cons (e1, e2) -> 
      let v1 = interpret_expression dynenv e1 in
      let v2 = interpret_expression dynenv e2 in
      Cons (v1, v2)
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
        (name, interpret_expression dynenv e)) bindings
      in interpret_expression (entries @ dynenv) body

  | Cond clauses -> 
      let rec eval = function 
      | [] -> raise (RuntimeError "cond expression ran out of clauses")
      | (pred, body) :: cs -> 
          match interpret_expression dynenv pred with
          | Bool false -> eval cs
          | _ -> interpret_expression dynenv body (* all other values are truthy *)
      in eval clauses 

  | Match _ -> expr

  | Call (e, args) -> begin
      match interpret_expression dynenv e with 
      | Closure (f, env) -> 
          if List.length args <> List.length f.lambda_param_names
          then raise (RuntimeError ("Incorrect number of arguments supplied to " ^ string_of_expr e));
          let vs = List.map (interpret_expression dynenv) args
          in let ext = List.combine f.lambda_param_names vs
          in let newenv = match f.rec_name with 
          | Some n -> (n, Closure (f, env)) :: ext
          | None   -> ext
          in 
          (* print_endline ("extended env to " ^ string_of_dynamic_env (newenv @ env)); *)
          interpret_expression (newenv @ env) f.lambda_body
      | _ -> raise (RuntimeError ("Expression not a function " ^ string_of_expr e)) (* TODO: fix this error message! *)
      (* match lookup dynenv e with (* callenv = current dynamic environment *) *)
      (* | Some (Closure (f, env)) ->  *)
      (*     if List.length args <> List.length f.param_names  *)
      (*     then raise (RuntimeError ("Incorrect number of arguments supplied to function " ^ name)); *)
      (*     let vals = List.map (fun v -> VariableEntry (interpret_expression dynenv v)) args  *)
      (*     in let extended_environment = (name, FunctionEntry (f, defenv)) :: (List.combine f.param_names vals) @ env  *)
      (*     in interpret_expression extended_environment f.body  *)
      (* | _ -> raise (RuntimeError ("Unbound function " ^ name)) *)
      end

  | Lambda args -> Closure (args, dynenv) 

  | StructConstructor (s, es) -> StructConstructor (s, List.map (interpret_expression dynenv) es)
  | StructPredicate (s, e) -> begin 
      match interpret_expression dynenv e with 
      (* we technically didn't cover then when guard language feature... but, the other way is syntactically noisy while this is cleaner; i am willing to lose points to reduce redundancy *)
      | StructConstructor (s', _) when String.equal s s' -> Bool true 
      | _ -> Bool false
      end
  | StructAccess (s, i, e) -> begin 
      match interpret_expression dynenv e with 
      | StructConstructor (s', vs) -> 
          if not (String.equal s s') then raise (RuntimeError ("Incorrect accessor for struct " ^ string_of_expr e))
          else if i >= List.length vs || i < 0 then raise (RuntimeError ("Invalid accessor index for struct " ^ string_of_expr e))
          else List.nth vs i
      | _ -> raise (RuntimeError ("Struct accessor '" ^ s ^ "' applied to non-struct " ^ string_of_expr e))
      end 

  | Print e -> e |> string_of_expr |> print_endline; Nil 


(* extend dynamic environment with binding *)
let interpret_binding dynenv b : dynamic_env =
  match b with
  | VarBinding (x, e) ->
      let v = interpret_expression dynenv e in
      Printf.printf "%s = %s\n%!" x (string_of_expr v);
      (x, v) :: dynenv

  | FunctionBinding f -> let func =
        { rec_name           = Some f.func_name 
        ; lambda_param_names = f.param_names 
        ; lambda_body        = f.body } in
        (f.func_name, Closure (func, dynenv)) :: dynenv

  | TopLevelExpr e ->
      let v = interpret_expression dynenv e in
      print_endline (string_of_expr v);
      dynenv
      
  | StructBinding s -> (* NOTE: might need to be a closure here to capture current environment? *)
      let func params body = Lambda { rec_name = None; lambda_param_names = params; lambda_body = body } in
      let constructor        = (s.struct_name          , func s.field_names (StructConstructor (s.struct_name, (List.map (fun n -> Var n) s.field_names)))) in
      let predicate          = (s.struct_name ^ "?"    , func ["x"]         (StructPredicate   (s.struct_name, Var "x"))                                  ) in
      let field_accessor f i = (s.struct_name ^ "-" ^ f, func ["x"]         (StructAccess      (s.struct_name, i, Var "x"))                               ) in  
      let (_, field_accessors) = List.fold_left_map (fun i f -> (i + 1, field_accessor f i) ) 0 s.field_names in
      constructor :: predicate :: field_accessors @ dynenv

  | TestBinding e -> begin
      match interpret_expression dynenv e with 
      | Bool true -> dynenv
      | _ -> raise (RuntimeError (string_of_expr e ^ " fails"))
      end


(* the semantics of a whole program (sequence of bindings) *)
let interpret_bindings = List.fold_left interpret_binding

(* starting from dynenv, first interpret the list of bindings in order. 
   then, in the resulting dynamic environment, interpret the expression and return its value *)
let interpret_expression_after_bindings dynenv bindings = interpret_bindings dynenv bindings |> interpret_expression 
