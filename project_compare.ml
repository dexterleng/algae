open Preprocessing

let negate f a = not (f a)

let rec list_files_recursively dir =
  let is_hidden d =
    try d.[0] == '.'
    with invalid_arg -> true
  in
  match Sys.is_directory dir with
    | true ->
      let children = Sys.readdir dir |> Array.to_list in
      children
        |> List.filter (negate is_hidden)
        |> List.map (Filename.concat dir)
        |> List.map list_files_recursively
        |> List.flatten
    | false ->
      [dir]

let list_folders dir =
  Sys.readdir dir
    |> Array.to_list
    |> List.map (Filename.concat dir)
    |> List.filter Sys.is_directory

type kgram = {
  length: int;
  start_index: int;
  hash: int;
}

type project_file = {
  file_name: string;
  file_content: string;
  selected_kgrams: kgram list;
}

type project = {
  project_name: string;
  files: project_file list;
}

type file_compare_result = {
  project_a_file: project_file;
  project_b_file: project_file;
  matching_kgrams: (kgram * kgram) list
}

type project_compare_result = {
  project_a: project;
  project_b: project;
  file_compare_results: file_compare_result list;
}

let build_project project_dir ~k ~w =
  let project_files = list_files_recursively project_dir
    |> List.filter (fun filename -> Filename.check_suffix filename ".java")
    |> List.map (fun file_name ->
      let file_content = Preprocessing.read_file file_name in
      let selected_kgrams = (Preprocessing.hash_file file_content k) 
        |> Winnowing.winnow w
        |> List.map (fun (hash, start_index) -> { hash; start_index; length = k; }) in
      { file_name; file_content; selected_kgrams; }
    )
  in
  { project_name = project_dir; files = project_files; }

let rec generate_pairs list =
  match list with
    | [] -> []
    | head::rest -> List.append (List.map (fun o -> (head, o)) rest)  (generate_pairs rest)

let rec generate_pairs_between_two_lists list_a list_b =
  match list_a with
    | [] -> []
    | head_a::rest_a -> List.append
      (List.map (fun b -> (head_a, b)) list_b)
      (generate_pairs_between_two_lists rest_a list_b)

let rec list_zip list_a list_b =
  match list_a, list_b with
    | head_a::rest_a, head_b::rest_b -> (head_a, head_b)::(list_zip rest_a rest_b)
    | _, _ -> []

let compare_files project_a_file project_b_file =
  let rec pair_kgrams_with_matching_hashes kgrams_a kgrams_b pairs =
    match kgrams_a with
    | kgram_a::kgrams_a_rest ->
      let matching_kgrams_b = List.filter (fun kgram_b -> kgram_b.hash = kgram_a.hash) kgrams_b in
      let new_pairs = pairs @ (List.map (fun kgram_b -> (kgram_a, kgram_b)) matching_kgrams_b) in
      pair_kgrams_with_matching_hashes kgrams_a_rest kgrams_b new_pairs
    | [] -> pairs
  in
  let matching_kgrams = pair_kgrams_with_matching_hashes project_a_file.selected_kgrams project_b_file.selected_kgrams [] in
  { project_a_file; project_b_file; matching_kgrams; }

let compare_projects project_a project_b =
  let file_pairs = generate_pairs_between_two_lists project_a.files project_b.files in
  let file_compare_results = List.map (fun (a, b) -> compare_files a b) file_pairs in
  { project_a; project_b; file_compare_results; }

let () =
  let k = 30 in
  let w = 40 in
  let projects_parent_dir = "./tests/OldPractTest/" in
  let project_dirs = list_folders projects_parent_dir in
  let projects = List.map (build_project ~k:k ~w:w) project_dirs in
  let project_pairs = generate_pairs projects in
  let project_compare_results = List.map (fun (a, b) -> compare_projects a b) project_pairs in

  project_compare_results |> List.iter (fun { project_a; project_b; file_compare_results; } ->
    Printf.printf "COMPARING PROJECT %s with PROJECT %s" project_a.project_name project_b.project_name;
    file_compare_results |> List.iter (fun { project_a_file; project_b_file; matching_kgrams; } ->
      Printf.printf "COMPARING FILE %s with FILE %s" project_a_file.file_name project_b_file.file_name;
      let kgrams_a = Preprocessing.get_file_positions project_a_file.file_content (matching_kgrams |> List.map fst |> List.map (fun kg -> kg.start_index)) k in
      let kgrams_b = Preprocessing.get_file_positions project_b_file.file_content (matching_kgrams |> List.map snd |> List.map (fun kg -> kg.start_index)) k in
      let zip_kgrams = list_zip kgrams_a kgrams_b in
      zip_kgrams |> List.iter (fun ((_, v1), (_, v2)) ->
        print_endline v1;
        print_endline v2;
        print_endline ""
      );
      print_endline ""
    );
  );