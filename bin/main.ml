open Core

let () =
  let n = In_channel.(input_line_exn stdin) in
  match int_of_string_opt n with
  | Some n -> Todo_swamp.Runner.run_n_queries n
  | None ->
      Out_channel.print_endline "You need to give count of commands to execute"
