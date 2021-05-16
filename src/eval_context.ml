(** Eval_environment have whole pointers and values, and function to evaluate between context and continuation. *)

module T = Type
module EP = Execution_pointer

type 'a state =
  | Finish_evaluation of 'a
  | Next_instruction
  | Popped            of T.data

type 'a t = {
  mutable continuation_pointer : 'a Continuation.t;
  mutable current_status : 'a;
  mutable execution_pointer : EP.t;
  mutable next_proc : 'a state;
}

include Eval_context_intf

let make (type s) (module S : Status with type t = s) (status : s) =
  (module struct
    type status = S.t

    type context = status t

    let instance =
      {
        continuation_pointer = Continuation.make ~status ~previous_continuation:None;
        current_status = S.clone status;
        execution_pointer = S.execution_pointer status |> EP.clone;
        next_proc = Next_instruction;
      }

    let next t =
      match t.next_proc with
      | Finish_evaluation v -> `Finished v
      | Next_instruction    -> (
          let current = t.execution_pointer |> EP.current in
          match current with
          | None   -> `End_instruction
          | Some v ->
              t.execution_pointer <- EP.next t.execution_pointer;
              `Continue v)
      | Popped v            ->
          let next_proc =
            match t.continuation_pointer.previous_continuation with
            | None   -> Finish_evaluation t.current_status
            | Some _ -> Next_instruction
          in
          t.next_proc <- next_proc;
          `Popped v

    let status { current_status; _ } = current_status

    let update_status t ~f = t.current_status <- f t.current_status

    let push_continuation t status =
      let current_status = S.clone t.current_status in
      S.set_execution_pointer t.execution_pointer current_status;
      let new_cont = Continuation.make ~previous_continuation:(Some t.continuation_pointer) ~status:current_status in
      t.continuation_pointer <- new_cont;
      t.current_status <- status;
      t.execution_pointer <- S.execution_pointer status |> EP.clone

    let pop_continuation t value =
      let old_status = t.continuation_pointer.current_status |> S.clone in
      let old_cont = t.continuation_pointer.previous_continuation in
      match old_cont with
      | None      ->
          t.next_proc <- Popped value;
          t.current_status <- S.clone old_status
      | Some cont ->
          t.continuation_pointer <- cont;
          t.current_status <- old_status;
          t.execution_pointer <- S.execution_pointer old_status |> EP.clone;
          t.next_proc <- Popped value
  end : Instance
    with type status = S.t)
