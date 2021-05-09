(** Eval_environment have whole pointers and values, and function to evaluate between context and continuation. *)

module T = Type
module E = Eval_stack

type 'a state =
  | Finish_evaluation of 'a
  | Start
  | Continue

type 'a t = {
  mutable continuation_pointer : 'a Continuation.t;
  mutable current_status : 'a;
  mutable next_proc : 'a state;
}

include Eval_context_intf

let make (type s) (module S : Status with type t = s) (status : s) =
  (module struct
    type status = S.t

    type context = {
      mutable continuation_pointer : status Continuation.t;
      mutable current_status : status;
      mutable next_proc : status state;
    }

    let instance =
      {
        continuation_pointer = Continuation.make ~status ~previous_continuation:None;
        current_status = S.clone status;
        next_proc = Start;
      }

    let next t = match t.next_proc with Finish_evaluation v -> `Finished v | _ -> `Continue

    let status { current_status; _ } = current_status

    let update_status t ~f = t.current_status <- f t.current_status

    let push_continuation t status =
      let current_status = S.clone t.current_status in
      let new_cont = Continuation.make ~previous_continuation:(Some t.continuation_pointer) ~status:current_status in
      t.continuation_pointer <- new_cont;
      t.current_status <- status

    let pop_continuation t =
      let old_status = t.continuation_pointer.current_status |> S.clone in
      let old_cont = t.continuation_pointer.previous_continuation in
      match old_cont with
      | None      ->
          t.next_proc <- Finish_evaluation old_status;
          t.current_status <- old_status
      | Some cont ->
          t.continuation_pointer <- cont;
          t.current_status <- old_status;
          t.next_proc <- Continue
  end : Instance
    with type status = S.t)
