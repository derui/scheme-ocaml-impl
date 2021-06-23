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
