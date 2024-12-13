let shell cmd =
  Printf.eprintf " $ %s\n" cmd;
  let status = Sys.command cmd in
  if status <> 0 then begin
    prerr_endline ("Common.shell Error: " ^ cmd);
    exit status
  end

let wrap_result f x =
  try Ok (f x) with
  | exn -> Error exn

let unwrap_result = function
  | Ok x -> x
  | Error exn -> raise exn

let file_using_r filename f =
  let read_ch = open_in filename in
  let result = wrap_result f read_ch in
  close_in read_ch;
  unwrap_result result

let file_using_w filename f =
  let write_ch = open_out filename in
  let result = wrap_result f write_ch in
  close_out write_ch;
  unwrap_result result

let read_lines filename =
  file_using_r filename begin fun read_ch ->
    let rec iter store =
      try iter (input_line read_ch :: store) with
      | End_of_file -> List.rev store
    in
    iter []
  end

let write_lines filename lines =
  file_using_w filename begin fun out_ch ->
    List.iter (Printf.fprintf out_ch "%s\n") lines
  end
