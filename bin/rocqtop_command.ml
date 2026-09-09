open Common

let about_count = ref 0

type conn = (in_channel * out_channel * in_channel)

let parse_prompt line =
  try
    Ok (Scanf.sscanf line "<prompt>%s < %d || %d < </prompt>" (fun s a b -> s))
  with
  | Scanf.Scan_failure msg -> Error line (* non prompt error *)
  | End_of_file ->
     Error
       (!%"Rocqtop_command.parse_prompt End_of_file: line: '%s'" line)

let read_until_prompt ?(msg="") stderr =
  let rec iter max_count stderrs =
    if max_count <= 0 then (Log.warn (!%"Rocqtop_command Max loop: %s" msg); List.rev stderrs) else
    (* wait the response *)
    let timeout_sec = 10. in
    match Command.input_line_with_timeout timeout_sec stderr with
    | Some line ->
       begin match parse_prompt line with
       | Ok prompt -> List.rev stderrs
       | Error err -> iter (max_count - 1) (err::stderrs)
       end
    | None -> Log.debug (!%"Rocqtop_command Timeout: %s" msg);
              iter (max_count - 1) stderrs
  in
  iter 100 []

let send (i, o, e) coq_command =
  Log.debug (!%"Rocqtop_command: %s" coq_command);
  Command.send o (coq_command ^ "\n"); flush o;
  (* When the command finishes executing, a prompt appears in stderr. Therefore,
     check the prompt before reading the execution results from standard output.
     c.f. https://github.com/rocq-prover/rocq/blob/V9.1.1/toplevel/coqloop.ml#L542-L544
   *)
  let stderrs = read_until_prompt ~msg:coq_command e |> String.concat "\n" in
  match Command.read_available i with
  | None -> Error "empty response from coq"
  | Some res -> Ok (res, stderrs)


let using ?(coqtop_bin = "coqtop -emacs") f =
  Command.using coqtop_bin (fun (i,o,e) ->
      ignore @@ Command.read_available i;
      f (i, o, e)
    )

let about conn ident =
  incr about_count;
  send conn (!%"About %s." ident)

let find_available_command () =
  if Command.is_command_available "rocq" then
    Some "rocq top"
  else if Command.is_command_available "coqtop" then
    Some "coqtop"
  else None
