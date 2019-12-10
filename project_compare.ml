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

(* (v1, p1) list, (v2, p2) list -> (p1, p2) list where v1 = v2 *)
let matching_positions v1 v2 =
  let rec intersection_helper lst1 lst2 common =
    match lst1 with
    | [] -> common
    | (hash_a, position_a)::t -> 
      let position_b_option = try Some(List.assoc hash_a lst2) with Not_found -> None in
      let new_common = match position_b_option with
        | Some(position_b) -> (position_a, position_b)::common 
        | None -> common
      in
      intersection_helper t lst2 new_common 
  in
  List.rev(intersection_helper v1 v2 [])

let compare_projects (project_a, project_b) =
  let file_pairs = generate_pairs_between_two_lists project_a project_b in
  let file_pair_to_hash_matches = file_pairs
    |> List.map (fun ((processed_file_a, positions_a), (processed_file_b, positions_b)) ->
      ((processed_file_a, processed_file_b), (matching_positions positions_a positions_b))) in
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
    let a_matches = Preprocessing.get_file_positions file_a (List.map fst matched_positions) in
    let b_matches = Preprocessing.get_file_positions file_b (List.map snd matched_positions) in
    list_zip a_matches b_matches
      |> List.iter (fun ((_, v1), (_, v2)) ->
        print_endline v1;
        print_endline v2;
        print_endline "";
      );
    print_endline "";
  );

