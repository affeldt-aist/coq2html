(* Transitive reduction over a list of edges keyed by node identifiers.
   Algorithm ported from the Lua script [etc/buildlibgraph] originally
   written by Maxime Dénès for math-comp, removed in math-comp PR #1534.
   Initial OCaml draft generated with GitHub Copilot, then reviewed and
   adapted by hand. *)

open Common

let transitive_reduction_by_key ~nodes ~key ~edges =
  let node_by_key = Hashtbl.create (List.length nodes) in
  List.iter (fun node -> Hashtbl.replace node_by_key (key node) node) nodes;

  let matrix = Hashtbl.create (List.length nodes) in
  let ensure_row node_key =
    match Hashtbl.find_opt matrix node_key with
    | Some row -> row
    | None ->
       let row = Hashtbl.create (List.length nodes) in
       Hashtbl.add matrix node_key row;
       row
  in

  List.iter (fun node -> ignore (ensure_row (key node))) nodes;
  List.iter (fun (src, dst) ->
      let src_key = key src in
      let dst_key = key dst in
      let row = ensure_row src_key in
      Hashtbl.replace row dst_key true) edges;

  (* Transitive closure (Floyd-Warshall style). *)
  List.iter (fun mid ->
      let mid_key = key mid in
      let mid_row = ensure_row mid_key in
      List.iter (fun src ->
          let src_key = key src in
          let src_row = ensure_row src_key in
          if Hashtbl.mem src_row mid_key then
            List.iter (fun dst ->
                let dst_key = key dst in
                if Hashtbl.mem mid_row dst_key then
                  Hashtbl.replace src_row dst_key true) nodes) nodes) nodes;

  (* Transitive reduction. *)
  List.iter (fun mid ->
      let mid_key = key mid in
      let mid_row = ensure_row mid_key in
      List.iter (fun src ->
          let src_key = key src in
          let src_row = ensure_row src_key in
          if Hashtbl.mem src_row mid_key then
            List.iter (fun dst ->
                let dst_key = key dst in
                if Hashtbl.mem mid_row dst_key then
                  Hashtbl.remove src_row dst_key) nodes) nodes) nodes;

  List.fold_left (fun store src ->
      let src_key = key src in
      let src_row = ensure_row src_key in
      List.fold_left (fun store dst ->
          let dst_key = key dst in
          if Hashtbl.mem src_row dst_key then
            match Hashtbl.find_opt node_by_key dst_key with
            | Some dst_node -> (src, dst_node) :: store
            | None -> store
          else store) store nodes) [] nodes
  |> List.rev
  |> list_uniq
