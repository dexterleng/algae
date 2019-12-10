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

let build_project project_dir =
  let project_files = list_files_recursively project_dir
    |> List.filter (fun filename -> Filename.check_suffix filename ".java")
  in
  let f file_dirs =
    let process_file file_dir =
        let processed_file = Preprocessing.process_file file_dir in
        let positions = Winnowing.winnow (Preprocessing.hash_file processed_file) 40 in
        (processed_file, positions)
    in
    List.map process_file file_dirs
  in
  f project_files
  

let rec generate_pairs list =
  match list with
    | [] -> []
    | head::rest -> List.append (List.map (fun o -> (head, o)) rest)  (generate_pairs rest)

let generate_project_pairs project_file_dict =
  Comparison.FileDict.to_list project_file_dict |> generate_pairs

let rec generate_pairs_between_two_lists list_a list_b =
  match list_a with
    | [] -> []
    | head_a::rest_a -> List.append
      (List.map (fun b -> (head_a, b)) list_b)
      (generate_pairs_between_two_lists rest_a list_b)

let compare_projects (project_a, project_b) =
  let file_pairs = generate_pairs_between_two_lists project_a project_b in
  let file_pair_to_hash_matches = file_pairs
    |> List.map (fun ((processed_file_a, positions_a), (processed_file_b, positions_b)) ->
      (* Comparison.intersection returns (v,p) of first param! *)
      ((processed_file_a, processed_file_b), (Comparison.intersection positions_a positions_b))) in
  file_pair_to_hash_matches

let rec list_zip list_a list_b =
  match list_a, list_b with
    | head_a::rest_a, head_b::rest_b -> (head_a, head_b)::(list_zip rest_a rest_b)
    | _, _ -> []

let () =
  let projects_parent_dir = "./tests/OldPractTest/" in
  let project_dirs = list_folders projects_parent_dir in
  let projects = project_dirs |> List.map build_project  in
  let project_pairs = generate_pairs projects in
  let project_comparison_results = project_pairs 
    |> List.map compare_projects
    |> List.flatten in
  project_comparison_results
  |> List.iter (fun ((file_a, file_b), matched_positions) ->
    print_endline "";
    Printf.printf "COMPARING %s with %s \n" file_a.file_name file_b.file_name;
    Preprocessing.get_file_positions file_a (List.map snd matched_positions)
      |> List.iter (fun (p, v) ->
        print_endline v;
        print_endline "";
      );
    print_endline "";
  );

