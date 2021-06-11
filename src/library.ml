module T = Type

module Export_map = Map.Make (struct
  type t = string

  let compare = Stdlib.compare
end)

type name = string list

type exports = string Export_map.t

type definitions = T.data Environment.t

type t = {
  name : name;
  exports : exports;
  definitions : definitions;
}

let show t =
  let exports = Export_map.bindings t.exports |> List.map fst |> String.concat " " in
  Printf.sprintf "(library (exports %s))" exports

let pp fmt t = Format.fprintf fmt "%s" @@ show t

let make name =
  assert (List.length name > 0);
  assert (List.for_all (fun v -> String.length v > 0) name);
  { name; exports = Export_map.empty; definitions = Environment.make [] }

let name { name; _ } = name

let exports { exports; _ } = Export_map.bindings exports |> List.map fst

(* manipulate library *)
let export ~symbol ?renamed t =
  assert (String.length symbol > 0);
  assert (Option.map (fun v -> String.length v > 0) renamed |> Option.value ~default:true);
  let export_name = Option.value ~default:symbol renamed in
  { t with exports = Export_map.add export_name symbol t.exports }

let define ~symbol ~data t =
  assert (String.length symbol > 0);
  Environment.set t.definitions ~key:symbol ~v:data;
  t

let as_environment t =
  let definitions =
    Export_map.to_seq t.exports
    |> Seq.map (fun (export_name, definition_name) ->
           let definition = Environment.get ~key:definition_name t.definitions in
           assert (Option.is_some definition);
           (export_name, Option.get definition))
    |> List.of_seq
  in
  Environment.make definitions
