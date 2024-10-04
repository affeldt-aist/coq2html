type t = string

let from_file filename = filename

let generate_file pngfile mapfile srcfile =
  Sys.command (Printf.sprintf "tred %s | dot -Tpng -o %s -Tcmapx -o %s" srcfile pngfile mapfile)
  |> ignore
