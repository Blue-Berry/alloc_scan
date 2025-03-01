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

(* Parser for `(alloc{...} <Bytes> ...)` *)
let alloc_parser =
  let* _ = string "(alloc{" in
  let* locs = location_parser in
  let* _ = ws in
  let* bytes = take_while1 Char.is_digit in
  return (List.map locs ~f:(fun (filepath, line, col_start, col_end) -> (filepath, line, col_start, col_end, int_of_string bytes)))
;;

let parse_alloc input =
  match parse_string ~consume:Prefix alloc_parser input with
  | Ok results -> results
  | Error err -> failwith ("Parsing error: " ^ err)
;;

let parse_allocs input =
  List.map (allocs_pos input) ~f:(fun i ->
    let sub_string = String.drop_prefix input i in
    parse_alloc sub_string)
;;

let parse_file file =
  let input = In_channel.read_all file in
  parse_allocs input
  |> List.fold ~init:[] ~f: List.append 
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

let () =
  (* Get argument *)
  let file_path = Array.get (Sys.get_argv ()) 1 in
  (* Parse file *)
  let output = parse_file file_path in
  print_endline output
;;

