open Common

let about_count = ref 0

type conn = (in_channel * out_channel * in_channel)

let parse_prompt line =
  try
    Some (Scanf.sscanf line "<prompt>%s < %d || %d < </prompt>" (fun s a b -> s))
  with
  | Scanf.Scan_failure msg ->
     Log.debug (!%"Coqtop_command.parse_prompt Scan_failure: '%s'" msg); None
  | End_of_file -> None

let wait_prompt ?(msg="") e =
  let rec iter max_count store =
    if max_count <= 0 then Log.warn (!%"Coqtop_command Max loop: %s" msg) else
    (* wait the response *)
    let timeout_sec = 10. in
    match Command.input_line_with_timeout timeout_sec e with
    | Some line ->
       begin match parse_prompt line with
       | Some p -> ()
       | None   -> iter (max_count - 1) store
       end
    | None -> Log.warn (!%"Coqtop_command Timeout: %s" msg)
  in
  iter 100 []

let send ?(wait=0.05) (i, o, e) coq_command =
  prerr_string (!%"Coqtop_command: %s" coq_command); flush stderr;
  Command.send o coq_command; flush o;
  wait_prompt ~msg:coq_command e;
  match Command.read_available i with
  | None -> Error "empty response from coq"
  | Some res ->
     Ok res

let exit conn =
  ignore @@ send conn "Quit.\n"

let using ?(coqtop_bin = "coqtop -emacs") f =
  Command.using coqtop_bin (fun (i,o,e) ->
      ignore @@ Command.read_available i;
      let y = f (i, o, e) in
      exit (i, o, e);
      y
    )

let about conn ident =
  incr about_count;
  send conn (!%"About %s.\n" ident)




