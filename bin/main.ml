open Core

let () =
  let n = In_channel.(input_line_exn stdin) in
  Todo_swamp.Runner.run_n_queries (int_of_string n)
