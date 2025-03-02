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

let highlight client namespace line col_start col_end =
  let buffer = Nvim_internal.Buffer.Or_current.Current in
  let hl_group : string = "Search" in
  let%bind changedtick = Vcaml.Buffer.get_changedtick [%here] client buffer in
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

let highlight_allocs client _cmm_file =
  let%bind buf_name = get_buffer_name client in
  let file_name = Filename.basename buf_name in
  let dir_name = Filename.dirname buf_name in
  let file_name_no_ext = Filename.chop_extension file_name in
  let dune_path = Find_dune.find_dune_project buf_name in
  let dir_name = Find_dune.relative_to_dune_project dir_name in
  let cmm_file = dune_path ^ "_build/default/" ^ dir_name ^ "/" ^ ("." ^ file_name_no_ext ^ ".objs/native/" ) ^ (file_name_no_ext ^ ".cmx.dump") in
  let%bind namespace = Namespace.create [%here] client ~name:"alloc-scan" () in
  let rec loop (locs : (string * int * int * int * int) list) =
    match locs with
    | [] -> return ()
    | (filepath, line, col_start, col_end, _) :: locs ->
      if String.is_substring buf_name ~substring:filepath
      then (
        let%bind _ = highlight client namespace (line - 1) col_start col_end in
        loop locs)
      else loop locs
  in
  let locs = Parsing.parse_file cmm_file in
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
