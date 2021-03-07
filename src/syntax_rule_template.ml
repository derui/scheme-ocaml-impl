module S = Syntax_rule_lib
module P = S.Pattern
module M = Syntax_rule_pattern_matcher
module T = Type

type cons =
  | E   of T.data
  | Dot of cons list

type cons_list = cons list

let rec cons_list_to_scheme list =
  let rec loop accum = function
    | []                    -> accum
    | Dot v' :: E v :: rest -> loop (T.Cons (v, cons_list_to_scheme v')) rest
    | E v :: rest           -> loop (T.Cons (v, accum)) rest
    | _                     -> failwith "Invalid list"
  in
  match list with [ E data ] -> data | _ -> loop T.Empty_list list

let expand matcher ellipsis template =
  let rec expand_list' matcher level accum template =
    match template with
    | T.Cons (T.Symbol e, T.Cons (v, T.Empty_list)) when e = ellipsis -> [ E v ]
    | T.Cons (v, T.Empty_list) ->
        let v = expand' matcher level v in
        List.fold_left (fun accum v -> v :: accum) accum v
    (* ellipsis at the end of list *)
    | T.Cons (v, T.Cons (T.Symbol e, T.Empty_list)) when e = ellipsis ->
        let v = expand' matcher (succ level) template in
        List.fold_left (fun accum v -> v :: accum) accum v
    (* ellipsis at the middle of list *)
    | T.Cons (v, T.Cons (T.Symbol e, (T.Cons _ as rest))) when e = ellipsis ->
        let v = expand' matcher (succ level) template in
        let accum = List.fold_left (fun accum v -> v :: accum) accum v in
        expand_list' matcher level accum rest
    (* ellipsis at before dot *)
    | T.Cons (v, T.Cons (T.Symbol e, rest)) when e = ellipsis ->
        let v = expand' matcher (succ level) template in
        let accum = List.fold_left (fun accum v -> v :: accum) accum v in
        let dot = expand' matcher level rest in
        Dot dot :: accum
    | T.Cons (v, rest) ->
        let v = expand' matcher level template in
        let accum = List.fold_left (fun accum v -> v :: accum) accum v in
        let v = expand' matcher level rest in
        List.fold_left (fun accum v -> v :: accum) accum v
    | _ -> failwith ""
  and expand' matcher level template =
    match template with
    | T.Symbol v -> (
        let pv = M.Pattern_matcher.get_pattern_variable (v, level) matcher in
        match pv with None -> [ E template ] | Some v -> List.map (fun v -> E v) v )
    | T.Cons _   -> expand_list' matcher level [] template
    | _          -> [ E template ]
  in
  let expanded = expand' matcher 0 template in
  cons_list_to_scheme expanded
