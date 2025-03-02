open Core

(* Starting at filepath, go up the directory tree until we find a dune-project file. *)
let find_dune_project filepath =
  let rec go_up dir =
    let dune_project = Filename.concat dir "dune-project" in
    if Sys_unix.file_exists_exn dune_project
    then dir
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
  | None -> failwithf "Could not find relative path: %s \n %s" filepath dune_project ()
  | Some relative_path -> relative_path
;;

let find_file_with_extension filepath extension =
  let files = Sys_unix.ls_dir filepath in
  let files = List.filter files ~f:(fun file -> Filename.check_suffix file extension) in
  match files with
  | [] -> failwithf "Could not find file with extension: %s \n %s" extension filepath ()
  | [ file ] -> file
  | _ -> failwithf "Found multiple files with extension: %s \n %s" extension filepath ()
;;
