let pattern_of_pst p =
  match p with
  | Pst.Symbol sym -> begin
      match int_of_string_opt sym with
      | Some n -> failwith "TODO: build an int pattern with n here"
      | None ->
         match sym with
         | "_" -> WildcardPattern
         | "true" -> failwith "TODO: build a bool pattern with true here"
         (* TODO: add other cases here for "false" and "nil" *)
         | _ ->
            if String.get sym 0 = '\'' (* if the string starts with an apostrophe *)
            then let sym_without_apostrophe = String.sub sym 1 (String.length sym - 1)
                 in failwith "TODO: build a symbol pattern using sym_without_apostrophe"
            else failwith "TODO: build a variable pattern using sym"
    end
  | Pst.Node [] -> raise (AbstractSyntaxError "Expected pattern but got '()'")
  | Pst.Node (head :: args) ->
     match head, args with
     | Pst.Symbol "cons", [p1; p2] -> ConsPattern (pattern_of_pst p1, pattern_of_pst p2)
     | Pst.Symbol s, ps -> failwith "TODO: build a struct pattern using patterns ps"
     | _ -> raise (AbstractSyntaxError ("Expected pattern, but got " ^ Pst.string_of_pst p))

let pattern_of_string s =
  s
  |> Pstparser.pst_of_string
  |> pattern_of_pst
