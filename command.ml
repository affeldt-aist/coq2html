open Common

let send o input =
  output_string o input; flush o

let read_available ?(max=4096) ch =
  let fd = Unix.descr_of_in_channel ch in
  let ready, _, _ = Unix.select [fd] [] [] 0.1 in
  if ready <> [] then begin
      let rec iter store =
        let buf = Bytes.create max in
        let len = input ch buf 0 max in
        if len < max then
          let s = Bytes.sub_string buf 0 len in
          String.concat "" @@ List.rev (s :: store)
        else
          let s = Bytes.to_string buf in
          iter (s :: store)
      in
      Some (iter [])
    end
  else None

let show_errors (i, o, e) =
  match read_available e with
  | Some msg -> Log.warn msg
  | None -> ()

let close command ioe =
  show_errors ioe;
  match Unix.close_process_full ioe with
  | Unix.WEXITED 0 -> ()
  | WEXITED other ->
     Log.warn (!%"Command '%s': exit %d" command other)
  | WSIGNALED signal ->
     Log.warn (!%"Command '%s': killed by a signal:%d" command signal)
  | WSTOPPED signal ->
     Log.warn (!%"Command '%s': stopped by a signal:%d" command signal)

let using command f =
  let env = Unix.environment () in
  let (i, o, e) = Unix.open_process_full command env in
  try
    let y = f (i, o, e) in
    close command (i, o, e);
    y
  with
  | exn ->
     close command (i, o, e);
     raise exn
