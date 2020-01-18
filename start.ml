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
        w = flag "-w" (required int) ~doc:"w"
      in
      fun () ->
          let valid_parent_directory = Sys.is_directory projects_dir in
          let valid_output_directory = Sys.is_directory output_dir in
          (if Stdlib.(<>) valid_parent_directory `Yes then failwith "projects-dir is not a valid directory.");
          (if Stdlib.(<>) valid_output_directory `Yes then failwith "output-dir not a valid directory.");
          Out_channel.write_all (Filename.concat output_dir "README.md") ~data: "This is a file created so we do not do a huge computation and find out you can't save in the output directory.";
          let projects = build_projects projects_dir ~k:k ~w:w in
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

