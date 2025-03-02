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


