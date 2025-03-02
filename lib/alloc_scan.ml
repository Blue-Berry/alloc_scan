open Core

let find_allocs (input : string) : int list =
  Core.String.substr_index_all input ~may_overlap:false ~pattern:"(alloc{"
;;

let allocs_pos input : int list =
  let allocs = find_allocs input in
  allocs
;;

open Angstrom

let ws =
  skip_while (function
    | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
    | _ -> false)
;;

let location_parser =
  let ( <> ) c1 c2 = not @@ Char.equal c1 c2 in
  let* filepath = take_while (fun c -> c <> ':') <* char ':' in
  let* line_num = take_while1 Char.is_digit <* char ',' in
  let* col_start = take_while1 Char.is_digit <* char '-' in
  let* col_end = take_while1 Char.is_digit in
  return (filepath, int_of_string line_num, int_of_string col_start, int_of_string col_end)
;;

let location_parser =
  let* locs = sep_by (char ';') location_parser in
  let* _ = char '}' in
  return locs
;;

(* Parser for `(alloc{...} <Bytes> ...)` *)
let alloc_parser =
  let* _ = string "(alloc{" in
  let* locs = location_parser in
  let* _ = ws in
  let* bytes = take_while1 Char.is_digit in
  return
    (List.map locs ~f:(fun (filepath, line, col_start, col_end) ->
       filepath, line, col_start, col_end, int_of_string bytes))
;;

let parse_alloc input =
  match parse_string ~consume:Prefix alloc_parser input with
  | Ok results -> results
  | Error err ->
    Out_channel.output_string Out_channel.stderr ("Parsing Error: " ^ err ^ "\n");
    []
;;

let parse_allocs input =
  List.map (allocs_pos input) ~f:(fun i ->
    let sub_string = String.drop_prefix input i in
    parse_alloc sub_string)
;;

let sort_by_file_and_line (allocs : (string * int * int * int * int) list) =
  List.sort
    ~compare:(fun (file1, line1, _, _, _) (file2, line2, _, _, _) ->
      if String.equal file1 file2
      then Int.compare line1 line2
      else String.compare file1 file2)
    allocs
;;

let combine_overlap_allocs (allocs : (string * int * int * int * int) list) =
  let rec loop acc = function
    | [] -> acc
    | [ x ] -> x :: acc
    | x1 :: x2 :: xs ->
      let file, line, col_start, col_end, bytes = x1 in
      let file', line', col_start', col_end', bytes' = x2 in
      if String.equal file file' && line = line'
      then
        loop
          acc
          ((file, line, min col_start col_start', max col_end col_end', bytes + bytes')
           :: xs)
      else loop (x1 :: acc) (x2 :: xs)
  in
  loop [] allocs
;;

let filter_empty allocs =
  allocs |> List.filter ~f:(fun (_, _, col_start, col_end, _) -> col_start <> col_end)
;;

let sort_and_combine_allocs allocs =
  allocs |> sort_by_file_and_line |> combine_overlap_allocs |> List.rev |> filter_empty
;;

let parse_file file =
  let input = In_channel.read_all file in
  parse_allocs input |> List.fold ~init:[] ~f:List.append |> sort_and_combine_allocs
;;

let allocs_string allocs =
  List.fold allocs ~init:"" ~f:(fun acc (filepath, line, col_start, col_end, bytes) ->
    acc
    ^ filepath
    ^ ":"
    ^ Int.to_string line
    ^ ","
    ^ Int.to_string col_start
    ^ "-"
    ^ Int.to_string col_end
    ^ ";"
    ^ Int.to_string bytes
    ^ "\n")
;;

open Vcaml
open Async.Deferred.Or_error.Let_syntax

let clear_highlights client =
  let%bind namespace = Namespace.create [%here] client ~name:"alloc-scan" () in
  Buffer.Untested.clear_namespace
    [%here]
    client
    Current
    ~namespace
    ~line_start:0
    ~line_end:(-1)
;;

let highlight client namespace line col_start col_end =
  let buffer = Nvim_internal.Buffer.Or_current.Current in
  let hl_group : string = "Search" in
  let%bind changedtick = Vcaml.Buffer.get_changedtick [%here] client buffer in
  let%bind _ =
    Nvim.out_writeln
      [%here]
      client
      (string_of_int line ^ ":" ^ string_of_int col_start ^ "-" ^ string_of_int col_end)
  in
  Vcaml.Buffer.Untested.add_highlight
    [%here]
    client
    buffer
    ~changedtick
    ~namespace
    ~hl_group
    ~line
    ~col_start
    ~col_end
;;

let get_buffer_name client =
  Vcaml.Buffer.get_name [%here] client Nvim_internal.Buffer.Or_current.Current
;;

let highlight_allocs client from_file =
  let%bind buf_name = get_buffer_name client in
  let%bind namespace = Namespace.create [%here] client ~name:"alloc-scan" () in
  let rec loop (locs : (string * int * int * int * int) list) =
    match locs with
    | [] -> return ()
    | (filepath, line, col_start, col_end, _) :: locs ->
      let file = String.split_on_chars filepath ~on:[ '/' ] |> List.last_exn in
      if String.is_substring buf_name ~substring:file
      then (
        let%bind _ = highlight client namespace (line - 1) col_start col_end in
        loop locs)
      else loop locs
  in
  let locs = parse_file from_file in
  loop locs
;;

let find_allocs =
  Vcaml_plugin.Oneshot.Rpc.create
    [%here]
    ~name:"allocScan"
    ~type_:Ocaml_from_nvim.Blocking.(String @-> return Nil)
      (* ~f:(fun ~client filepath -> Nvim.out_writeln [%here] client (parse_file filepath)) *)
    ~f:(fun ~client filepath -> highlight_allocs client filepath)
;;

let clear_allocs =
  Vcaml_plugin.Oneshot.Rpc.create
    [%here]
    ~name:"allocClear"
    ~type_:Ocaml_from_nvim.Blocking.(return Nil)
      (* ~f:(fun ~client filepath -> Nvim.out_writeln [%here] client (parse_file filepath)) *)
    ~f:(fun ~client -> clear_highlights client)
;;

(** This is an example of a "oneshot" plugin - the process is spawned to handle a single
    RPC request invoked via [rpcrequest]. *)
let command =
  Vcaml_plugin.Oneshot.create
    ~name:"alloc-scan"
    ~description:"Find allocs in a file"
    [ find_allocs; clear_allocs ]
;;
