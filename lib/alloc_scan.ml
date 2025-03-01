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

let parse_file file =
  let input = In_channel.read_all file in
  parse_allocs input
  |> List.fold ~init:[] ~f:List.append
  |> List.fold_left ~init:"" ~f:(fun acc s ->
    let filepath, line, col_start, col_end, bytes = s in
    acc
    ^ filepath
    ^ ":"
    ^ string_of_int line
    ^ ":"
    ^ string_of_int col_start
    ^ ":"
    ^ string_of_int col_end
    ^ ":"
    ^ string_of_int bytes
    ^ "\n")
;;

open Vcaml

let highlight () =
  let buffer = Nvim_internal.Buffer.Or_current.Current in
  let ns_id : int = -1 in
  let hl_group : string = "Search" in
  let line : int = 1 in
  let col_start : int = 1 in
  let col_end : int = 2 in
  let result = Nvim_internal.nvim_buf_add_highlight ~buffer ~ns_id ~hl_group ~line ~col_start ~col_end in
  result
;;


let find_allocs =
  Vcaml_plugin.Oneshot.Rpc.create
    [%here]
    ~name:"alloc-scan"
    ~type_:Ocaml_from_nvim.Blocking.(String @-> return Nil)
    ~f:(fun ~client filepath -> highlight () |> ignore; Nvim.out_writeln [%here] client (parse_file filepath))
;;

(** This is an example of a "oneshot" plugin - the process is spawned to handle a single
    RPC request invoked via [rpcrequest]. *)
let command =
  Vcaml_plugin.Oneshot.create
    ~name:"alloc-scan"
    ~description:"Find allocs in a file"
    [ find_allocs ]
;;
