open Core

(* Starting at filepath, go up the directory tree until we find a dune-project file. *)
let find_dune_project filepath =
  let rec go_up dir =
    let dune_project = Filename.concat dir "dune-project" in
    if Sys_unix.file_exists_exn dune_project
    then dune_project
    else (
      let parent = Filename.dirname dir ^ "/" in
      go_up parent)
  in
  go_up filepath
;;

let relative_to_dune_project filepath =
  let dune_project = find_dune_project filepath in
  let relative_path = String.chop_prefix ~prefix:dune_project filepath in
  match relative_path with
  | None -> failwith "Could not find relative path"
  | Some relative_path -> relative_path
;;
