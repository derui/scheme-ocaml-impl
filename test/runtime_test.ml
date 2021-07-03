module T = Ocaml_scheme.Type
module LP = Ocaml_scheme.Library_producer
module L = Ocaml_scheme.Library
module R = Ocaml_scheme.Runtime
module Pr = Ocaml_scheme.Printer
module FQ = Ocaml_scheme.Feature_query

let data = Alcotest.testable Pr.pp ( = )

let library_t = Alcotest.testable Fmt.nop ( = )

let tests =
  let library name = L.make name in

  [
    Alcotest.test_case "should return library if it exists" `Quick (fun () ->
        let lib1 = library [ "name" ] in
        let module Lp : LP.S = struct
          let all_list () = [ lib1 ]

          let produce name = List.find_opt (fun v -> L.name v = name) @@ all_list ()

          let exists _ = false
        end in
        let module Q : FQ.S = struct
          let is_implemented _ = false
        end in
        let module R = R.Make (Lp) (Q) in
        let actual = R.get_library [ "name" ] in
        let expected = Some lib1 in
        Alcotest.(check @@ option library_t) "lib" actual expected);
    Alcotest.test_case "should return none if it not exists" `Quick (fun () ->
        let lib1 = library [ "name" ] in
        let module Lp : LP.S = struct
          let all_list () = [ lib1 ]

          let produce name = List.find_opt (fun v -> L.name v = name) @@ all_list ()

          let exists _ = false
        end in
        let module Q : FQ.S = struct
          let is_implemented _ = false
        end in
        let module R = R.Make (Lp) (Q) in
        let actual = R.get_library [ "name"; "nested" ] in
        let expected = None in
        Alcotest.(check @@ option library_t) "lib" actual expected);
    Alcotest.test_case "should return none if it not exists" `Quick (fun () ->
        let lib1 = library [ "name" ] in
        let module Lp : LP.S = struct
          let all_list () = [ lib1 ]

          let produce name = List.find_opt (fun v -> L.name v = name) @@ all_list ()

          let exists _ = false
        end in
        let module Q : FQ.S = struct
          let is_implemented _ = false
        end in
        let module R = R.Make (Lp) (Q) in
        let actual = R.get_library [ "name"; "nested" ] in
        let expected = None in
        Alcotest.(check @@ option library_t) "lib" actual expected);
    Alcotest.test_case "should return library when it defines on demand" `Quick (fun () ->
        let lib1 = library [ "name" ] in
        let module Lp : LP.S = struct
          let all_list () = []

          let produce name = List.find_opt (fun v -> L.name v = name) @@ all_list ()

          let exists _ = false
        end in
        let module Q : FQ.S = struct
          let is_implemented _ = false
        end in
        let module R = R.Make (Lp) (Q) in
        R.define_library lib1;
        let actual = R.get_library [ "name" ] in
        let expected = Some lib1 in
        Alcotest.(check @@ option library_t) "lib" actual expected);
    Alcotest.test_case "should check requirement: a library is defined" `Quick (fun () ->
        let lib1 = library [ "name" ] in
        let module Lp : LP.S = struct
          let all_list () = [ lib1 ]

          let produce name = List.find_opt (fun v -> L.name v = name) @@ all_list ()

          let exists _ = true
        end in
        let module Q : FQ.S = struct
          let is_implemented _ = false
        end in
        let module R = R.Make (Lp) (Q) in
        R.define_library lib1;
        let actual = R.is_requirement_filled (FQ.Feature_requirement.Library [ "name" ]) in
        let expected = true in
        Alcotest.(check bool) "lib" actual expected);
    Alcotest.test_case "should check requirement: a feature is implemented" `Quick (fun () ->
        let lib1 = library [ "name" ] in
        let module Lp : LP.S = struct
          let all_list () = [ lib1 ]

          let produce name = List.find_opt (fun v -> L.name v = name) @@ all_list ()

          let exists _ = false
        end in
        let module Q : FQ.S = struct
          let is_implemented = function FQ.Feature_identifier.R7RS -> true | _ -> false
        end in
        let module R = R.Make (Lp) (Q) in
        R.define_library lib1;
        let actual = R.is_requirement_filled (FQ.Feature_requirement.Feature_identifier R7RS) in
        let expected = true in
        Alcotest.(check bool) "lib" actual expected);
    Alcotest.test_case "should check requirement: logical AND between 2 requirements" `Quick (fun () ->
        let lib1 = library [ "name" ] in
        let module Lp : LP.S = struct
          let all_list () = [ lib1 ]

          let produce name = List.find_opt (fun v -> L.name v = name) @@ all_list ()

          let exists _ = false
        end in
        let module Q : FQ.S = struct
          let is_implemented = function FQ.Feature_identifier.R7RS | Exact_closed -> true | _ -> false
        end in
        let module R = R.Make (Lp) (Q) in
        R.define_library lib1;
        let module F = FQ.Feature_identifier in
        let actual =
          R.is_requirement_filled
            FQ.Feature_requirement.(And [ Feature_identifier R7RS; Feature_identifier F.Exact_closed ])
        in
        let expected = true in
        Alcotest.(check bool) "lib" actual expected);
    Alcotest.test_case "should check requirement: logical OR between 2 requirements" `Quick (fun () ->
        let lib1 = library [ "name" ] in
        let module Lp : LP.S = struct
          let all_list () = [ lib1 ]

          let produce name = List.find_opt (fun v -> L.name v = name) @@ all_list ()

          let exists _ = false
        end in
        let module Q : FQ.S = struct
          let is_implemented = function FQ.Feature_identifier.R7RS -> true | _ -> false
        end in
        let module R = R.Make (Lp) (Q) in
        R.define_library lib1;
        let module F = FQ.Feature_identifier in
        let actual =
          R.is_requirement_filled
            FQ.Feature_requirement.(Or [ Feature_identifier R7RS; Feature_identifier F.Exact_closed ])
        in
        let expected = true in
        Alcotest.(check bool) "lib" actual expected);
    Alcotest.test_case "should check requirement: logical NOT a requirements" `Quick (fun () ->
        let lib1 = library [ "name" ] in
        let module Lp : LP.S = struct
          let all_list () = [ lib1 ]

          let produce name = List.find_opt (fun v -> L.name v = name) @@ all_list ()

          let exists _ = false
        end in
        let module Q : FQ.S = struct
          let is_implemented = function FQ.Feature_identifier.R7RS -> true | _ -> false
        end in
        let module R = R.Make (Lp) (Q) in
        R.define_library lib1;
        let module F = FQ.Feature_identifier in
        let actual = R.is_requirement_filled FQ.Feature_requirement.(!(Feature_identifier R7RS)) in
        let expected = false in
        Alcotest.(check bool) "lib" actual expected);
    Alcotest.test_case "should check requirement: combination some logical operations" `Quick (fun () ->
        let lib1 = library [ "name" ] in
        let module Lp : LP.S = struct
          let all_list () = [ lib1 ]

          let produce name = List.find_opt (fun v -> L.name v = name) @@ all_list ()

          let exists _ = true
        end in
        let module Q : FQ.S = struct
          let is_implemented = function FQ.Feature_identifier.R7RS -> true | _ -> false
        end in
        let module R = R.Make (Lp) (Q) in
        R.define_library lib1;
        let module F = FQ.Feature_identifier in
        let actual =
          R.is_requirement_filled
            FQ.Feature_requirement.(
              And [ Or [ Feature_identifier R7RS; Feature_identifier F.Ratios ]; Library [ "name" ] ])
        in
        let expected = true in
        Alcotest.(check bool) "lib" actual expected);
  ]
