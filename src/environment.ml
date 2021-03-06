type 'a t = {
  parent_env : 'a t option;
  bindings : (string, 'a) Hashtbl.t;
}

let make ?parent_env bindings =
  let bindings' = Hashtbl.create @@ List.length bindings in
  List.iter (fun (sym, binding) -> Hashtbl.add bindings' sym binding) bindings;
  { parent_env; bindings = bindings' }

let set env ~key ~v = Hashtbl.replace env.bindings key v

let replace env ~key ~v =
  let rec replace' env key =
    match env with
    | None     -> None
    | Some env ->
        let bindings = env.bindings in
        if not @@ Hashtbl.mem bindings key then replace' env.parent_env key
        else Hashtbl.replace bindings key v |> Option.some
  in
  replace' (Some env) key

let get env ~key =
  let rec get' env key =
    match env with
    | None     -> None
    | Some env -> ( match Hashtbl.find_opt env.bindings key with None -> get' env.parent_env key | Some v -> Some v)
  in
  get' (Some env) key

let merge ~base ~other =
  Hashtbl.to_seq other.bindings |> Hashtbl.add_seq base.bindings;
  base

let keys t =
  let keys = Hashtbl.to_seq_keys t.bindings |> List.of_seq in
  let keys' =
    t.parent_env
    |> Option.map (fun parent -> parent.bindings |> Hashtbl.to_seq_keys |> List.of_seq)
    |> Option.value ~default:[]
  in
  keys @ keys'
