module T = Type

module Cond_expand_clause = struct
  type t = {
    feature_requirement : Feature_query.Feature_requirement.t;
    expression : T.data;
  }

  let show v =
    let module Fr = Feature_query.Feature_requirement in
    Printf.sprintf "(%s %s)" (Fr.show v.feature_requirement) (Printer.print v.expression)

  let pp fmt v = Format.fprintf fmt "%s" @@ show v
end

type t = {
  clauses : Cond_expand_clause.t list;
  else_expression : T.data option;
}

let show v =
  let clauses = List.map Cond_expand_clause.show v.clauses |> String.concat " " in
  let else_expression = v.else_expression |> Option.map Printer.print |> Option.value ~default:"" in
  Printf.sprintf "(cond-expand %s %s)" clauses else_expression

let pp fmt v = Format.fprintf fmt "%s" @@ show v

let empty = { clauses = []; else_expression = None }

(** [eval cond_expand ~runtime] expand a cond-expand with [runtime] *)
let eval t ~runtime:(module R : Runtime.S) =
  let module Q = Feature_query in
  let clause =
    List.find_opt (fun clause -> R.is_requirement_filled clause.Cond_expand_clause.feature_requirement) t.clauses
  in
  match clause with None -> t.else_expression | Some clause -> Some clause.expression
