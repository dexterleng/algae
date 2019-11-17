let rec list_files dir =
  match Sys.is_directory dir with
    | true ->
      let children = Sys.readdir dir |> Array.to_list in
      children
        |> List.filter (fun c ->
          try c.[0] != '.'
          with invalid_arg -> false
        )
        |> List.map (Filename.concat dir)
        |> List.map list_files
        |> List.flatten
    | false ->
      [dir]

let build_project_file_dict project_dir =
  let project_files = list_files project_dir in
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
      ((file_a, file_b), (Comparison.intersection hashes_a hashes_b))) in
  Hashtbl.of_seq (List.to_seq file_pair_to_hash_matches)

let rec list_zip list_a list_b =
  match list_a, list_b with
    | head_a::rest_a, head_b::rest_b -> (head_a, head_b)::(list_zip rest_a rest_b)
    | _, _ -> []

let () =
  let a_dir = "./tests/test1" in
  let b_dir = "./tests/testx" in
  let p_a = build_project_file_dict a_dir in
  let p_b = build_project_file_dict b_dir in
  let compare_result = compare_projects p_a p_b in
  let compare_result_list = List.of_seq (Hashtbl.to_seq compare_result) in
  let string_matches = compare_result_list
    |> List.map (fun ((file_a, file_b), hashes) ->
      let positions = List.map snd hashes in
      let a_string_matches = Preprocessing.get_file_positions file_a positions |> List.map snd in
      let b_string_matches = Preprocessing.get_file_positions file_b positions |> List.map snd in
      let zipped_string_matches = list_zip a_string_matches b_string_matches in
      (file_a, file_b, zipped_string_matches)
    )
  in
  string_matches |>
  List.iter (fun (file_a, file_b, matches) ->
    if List.length matches > 0 then
      print_endline ("comparing " ^ file_a ^ " with " ^ file_b);
      print_endline "";
      matches |> List.iter (fun (match_a, match_b) ->
        print_endline "from a";
        print_endline match_a;
        print_endline "from b:";
        print_endline match_b;
        print_endline "";
      );
      print_endline ""
  )