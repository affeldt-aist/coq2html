open Common

let send o input =
  output_string o input; flush o

let read_available ?(max=4096) ch =
  let fd = Unix.descr_of_in_channel ch in
  let ready, _, _ = Unix.select [fd] [] [] 0.1 in
  if ready <> [] then begin
      let buf = Bytes.create max in
      let len = input ch buf 0 max in
      Some (Bytes.sub_string buf 0 len)
    end
  else None

let warn msg = prerr_endline msg

let show_errors (i, o, e) =
  match read_available e with
  | Some msg -> warn msg
  | None -> ()

let close command ioe =
  show_errors ioe;
  match Unix.close_process_full ioe with
  | Unix.WEXITED 0 -> ()
  | WEXITED other ->
     warn (!%"Command '%s': exit %d" command other)
  | WSIGNALED signal ->
     warn (!%"Command '%s': killed by a signal:%d" command signal)
  | WSTOPPED signal ->
     warn (!%"Command '%s': stopped by a signal:%d" command signal)

let using command f =
  let env = Unix.environment () in
  let (i, o, e) = Unix.open_process_full command env in
  show_errors (i,o,e);
  try
    let y = f (i, o, e) in
    close command (i, o, e);
    y
  with
  | exn ->
     close command (i, o, e);
     raise exn
