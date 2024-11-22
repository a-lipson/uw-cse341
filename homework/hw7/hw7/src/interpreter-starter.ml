let rec interpret_pattern pattern value =
  match pattern, value with
  | WildcardPattern, _ -> Some []
  | ConsPattern (p1, p2), Cons (v1, v2) -> begin
     match interpret_pattern p1 v1, interpret_pattern p2 v2 with
     | Some l1, Some l2 -> Some (l1 @ l2)
     | _ -> None
    end
  (* TODO: add cases for other kinds of patterns here *)
  | _ -> None
