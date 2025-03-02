open Core
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

let highlight client namespace line col_start col_end bytes =
  let buffer = Nvim_internal.Buffer.Or_current.Current in
  let hl_group : string = "Search" in
  let%bind changedtick = Vcaml.Buffer.get_changedtick [%here] client buffer in
  let start_inclusive = Position.{ row = line; col = col_start } in
  let end_exclusive = Position.{ row = line; col = col_end } in
  let virtual_text =
    [ Highlighted_text.Chunk.{ text = string_of_int bytes; hl_group = Some "Comment" } ]
  in
  Buffer.Untested.create_extmark
    [%here]
    client
    ~changedtick
    buffer
    ~namespace
    ~start_inclusive
    ~end_exclusive
    ~hl_group
    ~virtual_text
    ~virtual_text_pos:`Eol
    ~strict:false
    ()
;;

(* Vcaml.Buffer.Untested.add_highlight *)
(*   [%here] *)
(*   client *)
(*   buffer *)
(*   ~changedtick *)
(*   ~namespace *)
(*   ~hl_group *)
(*   ~line *)
(*   ~col_start *)
(*   ~col_end *)

let get_buffer_name client =
  Vcaml.Buffer.get_name [%here] client Nvim_internal.Buffer.Or_current.Current
;;

let cmm_file buf_name : string =
  let dir_name = Filename.dirname buf_name ^ "/" in
  let dune_path = Find_dune.find_dune_project buf_name in
  let dir_name = Find_dune.relative_to_dune_project dir_name in
  let ( ^/ ) = Filename.concat in
  let cmm_file_path = dune_path ^/ "_build/default/" ^/ dir_name in
  let objs_folder =
    match Find_dune.find_file_with_extension cmm_file_path ".objs" with
    | Some objs_folder -> objs_folder
    | None ->
      Find_dune.find_file_with_extension cmm_file_path ".eobjs" |> Option.value_exn
  in
  let cmm_file_path = cmm_file_path ^/ objs_folder ^/ "native/" in
  let cmm_file =
    Find_dune.find_file_with_extension cmm_file_path ".cmx.dump" |> Option.value_exn
  in
  let cmm_file = cmm_file_path ^/ cmm_file in
  cmm_file
;;

let highlight_allocs client =
  let%bind buf_name = get_buffer_name client in
  let%bind namespace = Namespace.create [%here] client ~name:"alloc-scan" () in
  let cmm_file = cmm_file buf_name in
  let rec loop (locs : (string * int * int * int * int) list) =
    match locs with
    | [] -> return ()
    | (filepath, line, col_start, col_end, bytes) :: locs ->
      if String.is_substring buf_name ~substring:filepath
      then (
        let%bind _ = highlight client namespace (line - 1) col_start col_end bytes in
        loop locs)
      else loop locs
  in
  let locs = Parsing.parse_file cmm_file in
  loop locs
;;

let highlight_allocs_from_file client filepath =
  let%bind buf_name = get_buffer_name client in
  let%bind namespace = Namespace.create [%here] client ~name:"alloc-scan" () in
  let rec loop (locs : (string * int * int * int * int) list) =
    match locs with
    | [] -> return ()
    | (filepath, line, col_start, col_end, bytes) :: locs ->
      let file = String.split_on_chars filepath ~on:[ '/' ] |> List.last_exn in
      if String.is_substring buf_name ~substring:file
      then (
        let%bind _ = highlight client namespace (line - 1) col_start col_end bytes in
        loop locs)
      else loop locs
  in
  let locs = Parsing.parse_file filepath in
  loop locs
;;

let find_allocs =
  Vcaml_plugin.Oneshot.Rpc.create
    [%here]
    ~name:"allocScan"
    ~type_:Ocaml_from_nvim.Blocking.(return Nil)
      (* ~f:(fun ~client filepath -> Nvim.out_writeln [%here] client (parse_file filepath)) *)
    ~f:(fun ~client -> highlight_allocs client)
;;

let find_allocs_from_file =
  Vcaml_plugin.Oneshot.Rpc.create
    [%here]
    ~name:"allocScanFile"
    ~type_:Ocaml_from_nvim.Blocking.(String @-> return Nil)
      (* ~f:(fun ~client filepath -> Nvim.out_writeln [%here] client (parse_file filepath)) *)
    ~f:(fun ~client filepath -> highlight_allocs_from_file client filepath)
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
    [ find_allocs; clear_allocs; find_allocs_from_file ]
;;
