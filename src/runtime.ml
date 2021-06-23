include Runtime_intf
module F = Feature_query

module Library_map = Map.Make (struct
  type t = Library.name

  let compare = Stdlib.compare
end)

module Make (Library_producer : Library_producer.S) (Feature_query : Feature_query.S) : S = struct
  type t = { mutable loaded_libraries : Library.t Library_map.t }

  let instance = { loaded_libraries = Library_map.empty }

  let get_library name =
    match Library_map.find_opt name instance.loaded_libraries with
    | Some library -> Some library
    | None         -> (
        match Library_producer.produce name with
        | Some library ->
            instance.loaded_libraries <- Library_map.add name library instance.loaded_libraries;
            Some library
        | None         -> None)

  let define_library library =
    let name = Library.name library in
    instance.loaded_libraries <- Library_map.add name library instance.loaded_libraries

  let is_requirement_filled requirement =
    let libraries = Library_producer.all_list () in
    let rec match_requirement cond =
      match cond with
      | F.Feature_requirement.Library name -> List.exists (fun v -> Library.name v = name) libraries
      | F.Feature_requirement.Feature_identifier v -> Feature_query.is_implemented v
      | F.Feature_requirement.And exps -> List.for_all match_requirement exps
      | F.Feature_requirement.Or exps -> List.exists match_requirement exps
      | F.Feature_requirement.Not v -> not @@ match_requirement v
    in
    match_requirement requirement
end
