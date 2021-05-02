open Type

type t = {
  env : binding Environment.t;
  previous_continuation : t option;
  context : Eval_context.t;
}

let make ~context ~previous_continuation ~env = { env; previous_continuation; context }
