open Core
open Project_compare

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
          (if Stdlib.(<>) valid_parent_directory `Yes then failwith "not a valid directory.");
          (if Stdlib.(<>) valid_output_directory `Yes then failwith "not a valid directory.");
          Out_channel.write_all (Filename.concat output_dir "README.md") ~data: "This is a file created so we do not do a huge computation and find out you can't save in the output directory.";
          let projects = build_projects projects_parent_dir in
          print_endline "Projects have been generated and kgrams have been selected. Performing comparisons.";
          let all_compare_result = compare_all_projects projects in
          print_endline "";
          List.iter ~f:(fun result_thunk ->
            let r = result_thunk () in
            let filename = Printf.sprintf "%s_%s.json" r.project_a.project_name r.project_b.project_name in
            let file_dir = Filename.concat output_dir filename in
            Yojson.Safe.to_file file_dir (project_compare_result_to_yojson r);
          ) all_compare_result
       )))

let () =
  Command.run ~version:"1.0" command

