module S = Syntax_rule_lib
module P = S.Pattern
module M = Syntax_rule_pattern_matcher
module T = Type

type expanded = T.data

let cons car cdr = T.Cons (car, cdr)

let reverse v = Primitive_op.List_op.reverse v |> Result.get_ok

let apply_expanded accum expanded = List.fold_left (fun accum v -> cons v accum) accum expanded

let expand matcher ellipsis template =
  let rec expand_list' matcher level accum template =
    match template with
    | T.Empty_list -> reverse accum
    | T.Cons (v, T.Empty_list) ->
        let v = expand' matcher level v in
        apply_expanded accum v |> reverse
    | T.Cons (T.Symbol e, T.Cons (v, T.Empty_list)) when e = ellipsis -> cons v accum |> reverse
    | T.Cons ((T.Cons _ as v), rest) ->
        let accum = expand' matcher level v |> apply_expanded accum in
        expand_list' matcher level accum rest
    (* ellipsis at the end of list *)
    | T.Cons (v, T.Cons (T.Symbol e, (T.Cons _ as rest))) when e = ellipsis ->
        let accum = expand' matcher (succ level) v |> apply_expanded accum in
        expand_list' matcher level accum rest
    (* ellipsis at before dot *)
    | T.Cons (v, T.Cons (T.Symbol e, rest)) when e = ellipsis ->
        let accum = expand' matcher (succ level) v |> apply_expanded accum |> reverse in
        let dot = expand' matcher level rest |> List.hd in
        cons accum dot
    | T.Cons (v, (T.Cons _ as rest)) ->
        let accum = expand' matcher level v |> apply_expanded accum in
        expand_list' matcher level accum rest
    | T.Cons (v, rest) ->
        let accum = expand' matcher level v |> apply_expanded accum |> reverse in
        let dot = expand' matcher level rest |> List.hd in
        cons accum dot
    | _ -> failwith ""
  and expand' matcher level template =
    match template with
    | T.Symbol v -> (
        let pv = M.Pattern_matcher.get_pattern_variable (v, level) matcher in
        match pv with None -> [ template ] | Some v -> v )
    | T.Cons _   -> [ expand_list' matcher level T.Empty_list template ]
    | _          -> [ template ]
  in
  expand' matcher 0 template |> List.hd
