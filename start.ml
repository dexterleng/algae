open Core
open Project_compare
          
type list_of_file_compare_results = file_compare_result list [@@deriving yojson]


let command =
  Command.basic
    ~summary:"MOSS-like plagiarism detector"
    ~readme:(fun () -> "More detailed information")
    Command.Let_syntax.(
      let%map_open
        projects_dir = flag "-projects-dir" (required string) ~doc:"directory containing all projects" and
        output_dir = flag "-output-dir" (required string) ~doc:"directory where json results will be saved" and
        k = flag "-k" (required int) ~doc:"k" and
        w = flag "-w" (required int) ~doc:"w" and
        file_types = flag "-file-type" (listed string) ~doc: "file types to be compared. (e.g. -file-type js -file-type ts -file-type jsx)." and
        ignored_dirs = flag "-ignore-dir" (listed string) ~doc: "folder names to be ignored. (e.g. -ignore-dir node_modules)" and
        m = flag "-m" (optional_with_default 10 int) ~doc: "maximum number of times a given passage may appear before it is ignored. default is 10"
      in
      fun () ->
          let valid_parent_directory = Sys.is_directory projects_dir in
          let valid_output_directory = Sys.is_directory output_dir in
          (if Stdlib.(<>) valid_parent_directory `Yes then failwith "projects-dir is not a valid directory.");
          (if Stdlib.(<>) valid_output_directory `Yes then failwith "output-dir not a valid directory.");
          Out_channel.write_all (Filename.concat output_dir "README.md") ~data: "This is a file created so we do not do a huge computation and find out you can't save in the output directory.";
          let base_project = build_base_project projects_dir ~k:k ~w:w ~file_types:file_types ~blacklisted_directories:ignored_dirs in
          let base_project_hashes = match base_project with
            | None -> None
            | Some(base_project) -> Some(build_hash_set_of_project base_project)
          in
          let projects = build_projects projects_dir ~k:k ~w:w ~file_types:file_types ~blacklisted_directories:ignored_dirs in
          let projects = match base_project_hashes with
            | None -> projects
            | Some(base_project_hashes) ->
              print_endline "Base project detected.";
              List.map projects ~f:(fun project -> filter_project_ngrams project base_project_hashes)
          in

          (* list of sets of hashes *)
          let hashes_by_project = List.map projects ~f:(fun project ->
              List.concat_map project.files ~f:(fun file -> Hashtbl.keys file.selected_kgrams_by_hash)
                |> Set.of_list (module Int)
          )
          in
          let hash_count = List.fold hashes_by_project ~init:(Map.empty (module Int)) ~f:(fun map hashes ->
            Set.fold hashes ~init:map ~f:(fun map hash ->
                let count = Map.find map hash in
                match count with
                  | None -> Map.set map ~key:hash ~data:1
                  | Some(count) -> Map.set map ~key:hash ~data:(count + 1)
            )
          )
          in
          (* hashes that exceed the max occurrence specified by the -m flag *)
          let freq_occuring_hashes = Set.of_map_keys (Map.filter hash_count ~f:(fun count -> count > m)) in
          let projects =
            List.map projects ~f:(fun project -> filter_project_ngrams project freq_occuring_hashes)
          in

          print_endline "Projects have been generated and kgrams have been selected. Performing comparisons.";
          let project_compare_result_thunks = compare_all_projects projects in
          let top_file_compare_results = top_k_file_compare_results project_compare_result_thunks ~k:1000 in
          
          (* save top k file compare results, where the file compare with the highest line count match is 1.json *)
          List.iteri top_file_compare_results ~f:(fun i r -> 
              let filename = Printf.sprintf "%d.json" i in
              let file_dir = Filename.concat output_dir filename in
              Yojson.Safe.to_file file_dir (file_compare_result_to_yojson { r with
                project_a_file = { r.project_a_file with selected_kgrams_by_hash = Hashtbl.create (module Int); };
                project_b_file = { r.project_b_file with selected_kgrams_by_hash = Hashtbl.create (module Int); };
              });
          );

          (* we need to save the top k in a single file for displaying a table so this is a lightweight version without kgrams *)
          let top_k_lightweight = List.map top_file_compare_results ~f:(fun r -> { r with
            matching_kgrams = [];
            project_a_file = { r.project_a_file with selected_kgrams_by_hash = Hashtbl.create (module Int); };
            project_b_file = { r.project_b_file with selected_kgrams_by_hash = Hashtbl.create (module Int); };
          }) in

          Yojson.Safe.to_file (Filename.concat output_dir "summary.json") (list_of_file_compare_results_to_yojson top_k_lightweight);
       )

let () =
  Command.run ~version:"1.0" command

