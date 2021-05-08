open Type

type t = {
  env : binding Environment.t;
  previous_continuation : t option;
  stack : Eval_stack.t;
}

let make ~stack ~previous_continuation ~env = { env; previous_continuation; stack }
