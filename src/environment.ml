type special_form_evaluator = Syntax.data Types.evaluation

type binding =
  | Value        of Syntax.data
  | Special_form of special_form_evaluator

type t = {
  parent_env : t option;
  bindings : (string, binding) Hashtbl.t;
}

let make ?parent_env bindings =
  let bindings' = Hashtbl.create @@ List.length bindings in
  List.iter (fun (sym, binding) -> Hashtbl.add bindings' sym binding) bindings;
  { parent_env; bindings = bindings' }

let set env ~key ~v =
  let rec set' env key =
    match env with
    | None     -> Types.raise_error @@ Printf.sprintf "%s is not defined" key
    | Some env ->
        let bindings = env.bindings in
        if not @@ Hashtbl.mem bindings key then set' env.parent_env key
        else (
          Hashtbl.replace bindings key v;
          Ok (Syntax.Symbol key) )
  in
  set' (Some env) key

let get env ~key =
  let rec get' env key =
    match env with
    | None     -> Types.raise_error @@ Printf.sprintf "unbound variable %s" key
    | Some env -> ( match Hashtbl.find_opt env.bindings key with None -> get' env.parent_env key | Some v -> Ok v )
  in
  get' (Some env) key
