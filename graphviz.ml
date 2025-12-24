open Common

type t = string
type dot = t

let from_file filename = filename

let generate_file pngfile mapfile srcfile =
  Common.shell (Printf.sprintf "tred %s | dot -Tpng -o %s -Tcmapx -o %s" srcfile pngfile mapfile)

let of_string body =
  let filename = "__tmp__.dot" in
  prerr_endline "Graphviz.of_string";
  file_using_w filename (fun ch -> output_string ch body);
  filename
