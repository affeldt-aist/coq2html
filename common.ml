let (!%) s = Printf.sprintf s

(* simple execution of external command *)
let shell cmd =
  Log.debug (!%" $ %s" cmd);
  let status = Sys.command cmd in
  if status <> 0 then begin
    Log.error ("Common.shell: " ^ cmd);
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

(* very naive brute force algorith *)
let strstr ~haystack ~needle =
  assert (needle <> "");
  let hlen = String.length haystack in
  let nlen = String.length needle in
  (* [has_prefix hpos npos] checks
     haystack.[hpos-npos+i] = needle.[i] for 0 <= i <= npos
  *)
  let rec has_prefix hpos npos =
    if haystack.[hpos] <> needle.[npos] then false
    else if npos = 0 then true
    else has_prefix (hpos - 1) (npos - 1)
  in
  let npos_init = nlen - 1 in
  let hlen_nlen = hlen - nlen in
  (* check from 0 to hlen - nlen *)
  let rec loop hstart =
    if hstart > hlen_nlen then None
    else
      if has_prefix (hstart + npos_init) npos_init then Some hstart
      else loop (hstart + 1)
  in
  loop 0

let grep word contents =
  strstr ~haystack:contents ~needle:word
  |> Option.is_some

let list_group_by f xs =
  let rec iter store = function
    | [] -> store
    | x :: xs ->
       let grp = f x in
       let mems   = List.filter (fun x -> grp =  f x) xs in
       let others = List.filter (fun x -> grp <> f x) xs in
       iter ((grp, x :: mems) :: store) others
  in
  iter [] xs

let list_sort_by f xs =
  List.sort (fun x y -> compare (f x) (f y)) xs

let list_uniq xs =
  List.fold_left (fun store x ->
      if List.mem x store then store else x :: store) [] xs
  |> List.rev

let list_take n xs =
  let rec iter store = function
    | (n, _) when n <= 0 -> List.rev store
    | (n, []) -> List.rev store
    | (n, x :: xs) -> iter (x :: store) (n - 1, xs)
  in
  iter [] (n, xs)

let list_drop n xs =
  let rec iter = function
    | (n, xs) when n <= 0 -> xs
    | (n, []) -> []
    | (n, _ :: xs) -> iter (n - 1, xs)
  in
  iter (n, xs)

let list_max_by measure xs =
  match xs with
  | [] -> None
  | x0 :: xs ->
     List.fold_left (fun (m, y) x -> if measure x > m then (measure x, x) else (m, y))
       (measure x0, x0) xs
     |> snd
     |> Option.some

let html_escaped =
  let buff = Buffer.create 5 in
  fun s ->
  Buffer.clear buff;
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '<' -> Buffer.add_string buff "&lt;"
    | '>' -> Buffer.add_string buff "&gt;"
    | '&' -> Buffer.add_string buff "&amp;"
    | '\"' -> Buffer.add_string buff "&quot;"
    | c -> Buffer.add_char buff c
  done;
  Buffer.contents buff
