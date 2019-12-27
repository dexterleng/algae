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
  selected_kgrams: kgram list;
} [@@deriving yojson]

type project = {
  project_name: string;
  project_dir: string;
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

let cmp_kgram k1 k2 = k1.hash - k2.hash

let build_project project_dir ~k ~w =
  let project_name = Filename.basename project_dir in
  let project_files = list_files_recursively project_dir
    |> List.filter ~f:(fun filename -> Filename.check_suffix filename ".java")
    |> List.map ~f:(fun file_name ->
      let lines = In_channel.read_lines file_name in
      let kgrams = Preprocessing.k_grams_with_line_number lines k in
      let selected_kgrams = Winnowing.winnow kgrams ~w:w ~cmp:cmp_kgram in
      { file_name; selected_kgrams; }
    )
  in
  { project_name; project_dir; files = project_files; }

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

let compare_two_projects project_a project_b =
  let file_pairs = generate_pairs_between_two_lists project_a.files project_b.files in
  let file_compare_results = List.map ~f:(fun (a, b) -> compare_files a b) file_pairs in
  { project_a; project_b; file_compare_results; }

let compare_all_projects projects =
  let project_pairs = generate_pairs projects in
  let all_compare_result = List.map ~f:(fun (a, b) -> compare_two_projects a b) project_pairs in
  all_compare_result

let build_projects projects_parent_dir =
  let project_dirs = list_folders projects_parent_dir in
  let projects = List.map ~f:(build_project ~k:10 ~w:10) project_dirs in
  projects

let command =
  Command.basic
    ~summary:"MOSS-like plagiarism detector"
    ~readme:(fun () -> "More detailed information")
    Command.Param.(
      map (both
        (anon ("parent directory of all projects" %: string))
        (anon ("json result output directory" %: string)))
       ~f:(fun (projects_parent_dir, output_dir) -> (fun () ->
          let valid_parent_directory = Sys.is_directory projects_parent_dir in
          let valid_output_directory = Sys.is_directory output_dir in
          (if valid_parent_directory <> `Yes then failwith "not a valid directory.");
          (if valid_output_directory <> `Yes then failwith "not a valid directory.");
          Out_channel.write_all (Filename.concat output_dir "README.md") ~data: "This is a file created so we do not do a huge computation and find out you can't save in the output directory.";
          let projects = build_projects projects_parent_dir in
          let all_compare_result = compare_all_projects projects in
          List.iter ~f:(fun r ->
            let filename = Printf.sprintf "%s_%s.json" r.project_a.project_name r.project_b.project_name in
            let file_dir = Filename.concat output_dir filename in
            Yojson.Safe.to_file file_dir (project_compare_result_to_yojson r);
          ) all_compare_result
       )))

let () =
  Command.run ~version:"1.0" command

(* let () =
  for i = 0 to Array.length Sys.argv - 1 do
    printf "[%i] %s\n" i Sys.argv.(i)
  done;
  let projects_parent_dir = "./tests/OldPractTest/" in
  let projects = build_projects projects_parent_dir in
  let all_compare_result = compare_all_projects projects in
  List.iter ~f:(fun r ->
    let file_dir = Printf.sprintf "./results/%s_%s.json" r.project_a.project_name r.project_b.project_name in
    Yojson.Safe.to_file file_dir (project_compare_result_to_yojson r);
  ) all_compare_result *)