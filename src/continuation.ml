type 'a t = {
  previous_continuation : 'a t option;
  current_status : 'a;
}

let make ~status ~previous_continuation = { current_status = status; previous_continuation }
