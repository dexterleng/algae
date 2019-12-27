open Preprocessing
open Core

let negate f a = not (f a)

let rec list_files_recursively dir =
  let is_hidden d = d.[0] = '.'
  in
  match Sys.is_directory dir with
    | `Yes ->
      let children = Sys.readdir dir |> Array.to_list in
      children
        |> List.filter ~f:(negate is_hidden)
        |> List.map ~f:(Filename.concat dir)
        |> List.concat_map ~f:list_files_recursively
    | `No -> [dir]
    | `Unknown -> [dir]

let list_folders dir =
  Sys.readdir dir
    |> Array.to_list
    |> List.map ~f:(Filename.concat dir)
    |> List.filter ~f: (Core.Fn.compose (fun is_dir -> is_dir = `Yes) Sys.is_directory)

type project_file = {
  file_name: string;
  lines: string list;
  selected_kgrams: kgram list;
} [@@deriving yojson]

type project = {
  project_name: string;
  files: project_file list;
} [@@deriving yojson] 

type file_compare_result = {
  project_a_file: project_file;
  project_b_file: project_file;
  matching_kgrams: (kgram * kgram) list;
  (* match density is no. of matching kgrams / total no. of kgrams of the file. *)
  project_a_file_match_density: float;
  project_b_file_match_density: float;
} [@@deriving yojson]

type project_compare_result = {
  project_a: project;
  project_b: project;
  file_compare_results: file_compare_result list;
} [@@deriving yojson]

let build_project project_dir ~k ~w =
  let project_files = list_files_recursively project_dir
    |> List.filter ~f:(fun filename -> Filename.check_suffix filename ".java")
    |> List.map ~f:(fun file_name ->
      let lines = In_channel.read_lines file_name in
      let kgrams = Preprocessing.k_grams_with_line_number lines k in
      let selected_kgrams = Winnowing.winnow kgrams ~w:w in
      { file_name; lines; selected_kgrams; }
    )
  in
  { project_name = project_dir; files = project_files; }

let rec generate_pairs list =
  match list with
    | [] -> []
    | head::rest -> List.append (List.map ~f:(fun o -> (head, o)) rest)  (generate_pairs rest)

let rec generate_pairs_between_two_lists list_a list_b =
  match list_a with
    | [] -> []
    | head_a::rest_a -> List.append
      (List.map ~f:(fun b -> (head_a, b)) list_b)
      (generate_pairs_between_two_lists rest_a list_b)

let rec list_zip list_a list_b =
  match list_a, list_b with
    | head_a::rest_a, head_b::rest_b -> (head_a, head_b)::(list_zip rest_a rest_b)
    | _, _ -> []

let compare_files project_a_file project_b_file =
  let rec pair_kgrams_with_matching_hashes kgrams_a kgrams_b pairs =
    match kgrams_a with
    | kgram_a::kgrams_a_rest ->
      let matching_kgrams_b = List.filter ~f:(fun kgram_b -> kgram_b.hash = kgram_a.hash) kgrams_b in
      let new_pairs = pairs @ (List.map ~f:(fun kgram_b -> (kgram_a, kgram_b)) matching_kgrams_b) in
      pair_kgrams_with_matching_hashes kgrams_a_rest kgrams_b new_pairs
    | [] -> pairs
  in
  let matching_kgrams = pair_kgrams_with_matching_hashes project_a_file.selected_kgrams project_b_file.selected_kgrams [] in
  let project_a_file_match_density = float_of_int (List.length matching_kgrams) /. float_of_int (List.length project_a_file.selected_kgrams) in
  let project_b_file_match_density = float_of_int (List.length matching_kgrams) /. float_of_int (List.length project_b_file.selected_kgrams) in
  { project_a_file; project_b_file; matching_kgrams; project_a_file_match_density; project_b_file_match_density; }

let compare_projects project_a project_b =
  let file_pairs = generate_pairs_between_two_lists project_a.files project_b.files in
  let file_compare_results = List.map ~f:(fun (a, b) -> compare_files a b) file_pairs in
  { project_a; project_b; file_compare_results; }

