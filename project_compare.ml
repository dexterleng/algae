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

let is_supported_file dir =
  let language_file =
    try Some(Preprocessing.determine_language_file dir)
    with _ -> None
  in
  match language_file with
    | Some(_) -> true
    | None -> false

let list_folders dir =
  Sys.readdir dir
    |> Array.to_list
    |> List.map (Filename.concat dir)
    |> List.filter Sys.is_directory 

let build_project_file_dict project_dir =
  let project_files = list_files_recursively project_dir
    |> List.filter is_supported_file
  in

  project_files |> List.iter (fun f ->
    print_endline f
  );

  let rec build_file_dict dirs file_dict =
    match dirs with
      | [] -> file_dict
      | dir::rest ->
        let new_dict = Comparison.FileDict.insert dir
          (Winnowing.winnow (Preprocessing.hash_file dir) 40) file_dict in
        build_file_dict rest new_dict
  in
  build_file_dict project_files Comparison.FileDict.empty

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

let compare_projects project_a_file_dict project_b_file_dict =
  let project_a = Comparison.FileDict.to_list project_a_file_dict in
  let project_b = Comparison.FileDict.to_list project_b_file_dict in
  let file_pairs = generate_pairs_between_two_lists project_a project_b in
  let file_pair_to_hash_matches = file_pairs
    |> List.map (fun ((file_a, hashes_a), (file_b, hashes_b)) ->
      (* Comparison.intersection returns (v,p) of first param! *)
      ((file_a, file_b), (Comparison.intersection hashes_a hashes_b))) in
  file_pair_to_hash_matches
    |> List.to_seq
    |> Hashtbl.of_seq

let rec list_zip list_a list_b =
  match list_a, list_b with
    | head_a::rest_a, head_b::rest_b -> (head_a, head_b)::(list_zip rest_a rest_b)
    | _, _ -> []

let () =
  let projects_parent_dir = "./tests/OldPractTest/DBIT_1A21_AY0809S2_JPRG_PRACTICAL_TEST/" in
  let project_dirs = list_folders projects_parent_dir in

  let project_file_dicts =
    project_dirs
      |> List.map (fun dir ->
        (dir, build_project_file_dict dir)
      )
  in
  let project_file_dict_pairs = generate_pairs project_file_dicts in

  let all_compare_results =
    project_file_dict_pairs
      |> List.map (fun ((a_dir, a_file_dict), (b_dir, b_file_dict)) ->
        let single_compare_result = compare_projects a_file_dict b_file_dict |> Hashtbl.to_seq |> List.of_seq in

        let string_matches = single_compare_result
          |> List.map (fun ((file_a, file_b), hashes) ->
            let positions = List.map snd hashes in
            
            positions |> List.iter (Printf.printf "%d ");

            let a_string_matches = Preprocessing.get_file_positions file_a positions |> List.map snd in
            ((a_dir, file_a), (b_dir, file_b), a_string_matches)
          )
        in
        string_matches
      )
      |> List.flatten
  in

  all_compare_results
    |> List.iter (fun (((a_dir, file_a), (b_dir, file_b), matches)) ->
      Printf.printf "COMPARING %s (%s) with %s (%s)\n" file_a a_dir file_b b_dir;
      print_endline "";
      matches |> List.iter (fun str ->
        print_endline str;
        print_endline "";
      );
      print_endline "";
    );