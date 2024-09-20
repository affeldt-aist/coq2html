type t = string

let from_file filename = filename

let generate_file dstfile srcfile =
  Sys.command (Printf.sprintf "tred %s | dot -Tsvg -o %s" srcfile dstfile)
  |> ignore
