let debug_flag = ref false

let debug message =
  if !debug_flag then prerr_endline ("Debug: " ^ message)

let warn message = prerr_endline ("Warning: " ^ message)
let error message = prerr_endline ("Error: " ^ message)


