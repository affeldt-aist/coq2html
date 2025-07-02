open Common

let about_count = ref 0

type conn = (in_channel * out_channel * in_channel)

let send ?(wait=0.05) (i, o, e) coq_command =
  Command.send o coq_command;
  Unix.sleepf wait;
  match Command.read_available i with
  | None -> Error "empty response from coq"
  | Some res ->
     Ok res

let exit conn =
  ignore @@ send conn "Quit."

let using ?(coqtop_bin = "coqtop") f =
  Command.using coqtop_bin (fun (i,o,e) ->
      ignore @@ Command.read_available i;
      let y = f (i, o, e) in
      exit (i, o, e);
      y
    )

let about conn ident =
  incr about_count;
  send conn (!%"About %s.\n" ident)




