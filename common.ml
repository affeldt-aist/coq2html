let shell cmd =
  Printf.eprintf " $ %s\n" cmd;
  let status = Sys.command cmd in
  if status <> 0 then begin
    prerr_endline ("Common.shell Error: " ^ cmd);
    exit status
  end
