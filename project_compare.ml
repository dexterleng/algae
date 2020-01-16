open Preprocessing
open Core

let negate f a = not (f a)

let rec list_files_recursively dir =
  let is_hidden d = Stdlib.(=) d.[0] '.'
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
    |> List.filter ~f: (Core.Fn.compose (fun is_dir -> Stdlib.(=) is_dir `Yes) Sys.is_directory)

type kgrams_by_hash = (int, kgram list) Hashtbl.t 

let kgrams_by_hash_to_yojson m =
    Hashtbl.to_alist m |>
    [%to_yojson: (int * kgram list) list] 

let kgrams_by_hash_of_yojson (_: Yojson.Safe.t) = Error "You cannot deserialize now!"

type project_file = {
  file_dir: string;
  selected_kgrams_by_hash: kgrams_by_hash;
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

  project_a_file_line_matches: int;
  project_b_file_line_matches: int;
} [@@deriving yojson]

type project_compare_result = {
  project_a: project;
  project_b: project;
  file_compare_results: file_compare_result list;
} [@@deriving yojson]

let cmp_kgram k1 k2 = k1.hash - k2.hash

let build_kgram_by_hash_map kgrams =
    let kgrams_paired_with_hash = List.map kgrams ~f:(fun g -> (g.hash, g)) in
    let map = Hashtbl.of_alist_multi (module Int) kgrams_paired_with_hash in
    map

let build_project project_dir ~k ~w =
  let project_name = Filename.basename project_dir in
  let project_files = list_files_recursively project_dir
    |> List.filter ~f:(fun filename -> Filename.check_suffix filename ".java")
    |> List.map ~f:(fun file_dir ->
      let file_dir_from_project_root = Filename.concat project_name (String.chop_prefix_exn file_dir ~prefix:project_dir) in
      let lines = In_channel.read_lines file_dir in
      let doc = Preprocessing.convert_to_document lines in
      let kgrams = Preprocessing.generate_n_gram_from_document doc k in
      let selected_kgrams = Winnowing.winnow kgrams ~w:w ~cmp:cmp_kgram in
      let selected_kgrams_by_hash = build_kgram_by_hash_map selected_kgrams in
      { file_dir = file_dir_from_project_root; selected_kgrams_by_hash; }
    )
  in
  { project_name; project_dir; files = project_files; }

let intersect_int_lists l1 l2 =
    let s1 = Set.of_list (module Int) l1 in
    let s2 = Set.of_list (module Int) l2 in
    Set.to_list (Set.inter s1 s2)
    
let generate_range_set s e =
    let rec fn s e res = 
      if s > e then
        Set.of_list (module Int) res
      else
        fn (s + 1) e (s::res)
    in
    fn s e []


let calculate_line_matches kgrams_a_by_hash kgrams_b_by_hash =
  let fn kgrams =
      let lines_set = List.fold_left kgrams ~init:(Set.empty (module Int)) ~f:(fun s -> fun k ->
        let range = generate_range_set (k.starting_line) (k.starting_line + k.lines_occupied - 1) in
        Set.union s range
      )
      in
      Set.length lines_set
  in
  let hashes_a = Hashtbl.keys kgrams_a_by_hash in
  let hashes_b = Hashtbl.keys kgrams_b_by_hash in
  let intersecting_hashes = intersect_int_lists hashes_a hashes_b in
  let lines_occupied_a = fn (List.concat_map intersecting_hashes ~f:(Hashtbl.find_exn kgrams_a_by_hash)) in
  let lines_occupied_b = fn (List.concat_map intersecting_hashes ~f:(Hashtbl.find_exn kgrams_b_by_hash)) in
  (lines_occupied_a, lines_occupied_b)

let calculate_match_density kgrams_a_by_hash kgrams_b_by_hash =
  let hashes_a = Hashtbl.keys kgrams_a_by_hash in
  let hashes_b = Hashtbl.keys kgrams_b_by_hash in
  let intersecting_hashes = intersect_int_lists hashes_a hashes_b in

  let calculate_number_of_kgrams kgrams_by_hash =
      let nested_kgrams = Hashtbl.to_alist kgrams_by_hash |> List.map ~f:snd in
      let kgram_count = List.fold_left nested_kgrams ~init:0 ~f:(fun count -> fun kgrams -> count + List.length kgrams) in   
      kgram_count
  in

  let rec fn hashes match_count_a match_count_b =
      match hashes with
      | hash::rest ->
        let matching_kgrams_a = match (Hashtbl.find kgrams_a_by_hash hash) with
            | Some(x) -> x
            | None -> []
        in
        let matching_kgrams_b = match (Hashtbl.find kgrams_b_by_hash hash) with
            | Some(x) -> x
            | None -> []
        in
        fn rest (match_count_a + List.length matching_kgrams_a) (match_count_b + List.length matching_kgrams_b)
      | [] ->
        let density_a = (float_of_int match_count_a) /. (float_of_int (calculate_number_of_kgrams kgrams_a_by_hash)) in
        let density_b = (float_of_int match_count_b) /. (float_of_int (calculate_number_of_kgrams kgrams_b_by_hash)) in
        let density_a_no_nan = if Float.is_nan density_a then 0.0 else density_a in
        let density_b_no_nan = if Float.is_nan density_b then 0.0 else density_b in
        (density_a_no_nan, density_b_no_nan)
  in
  fn intersecting_hashes 0 0

let compare_files project_a_file project_b_file =
  let pair_matching_kgrams kgrams_a_by_hash kgrams_b_by_hash =
    let hashes_a = Hashtbl.keys kgrams_a_by_hash in
    let hashes_b = Hashtbl.keys kgrams_b_by_hash in
    let intersecting_hashes = intersect_int_lists hashes_a hashes_b in

    List.concat_map intersecting_hashes ~f:(fun hash ->
        let matching_kgrams_a = match (Hashtbl.find kgrams_a_by_hash hash) with
            | Some(x) -> x
            | None -> []
        in
        let matching_kgrams_b = match (Hashtbl.find kgrams_b_by_hash hash) with
            | Some(x) -> x
            | None -> []
        in
    List.filter (List.cartesian_product matching_kgrams_a matching_kgrams_b) ~f:(fun (a, b) -> String.(=) a.str b.str)
    )
  in
  
  let matching_kgrams = pair_matching_kgrams project_a_file.selected_kgrams_by_hash project_b_file.selected_kgrams_by_hash in
  let (project_a_file_match_density, project_b_file_match_density) = calculate_match_density project_a_file.selected_kgrams_by_hash project_b_file.selected_kgrams_by_hash in
  let (project_a_file_line_matches, project_b_file_line_matches) = calculate_line_matches project_a_file.selected_kgrams_by_hash project_b_file.selected_kgrams_by_hash in
  { project_a_file; project_b_file; matching_kgrams; project_a_file_match_density; project_b_file_match_density; project_a_file_line_matches; project_b_file_line_matches; }

let compare_two_projects project_a project_b =
  let file_pairs = List.cartesian_product project_a.files project_b.files in
  let file_compare_results = List.map ~f:(fun (a, b) -> compare_files a b) file_pairs in
  { project_a; project_b; file_compare_results; }

let generate_pairs list =
  let rec fn list result =
    match list with
    | [] -> result 
    | head::rest -> fn rest ((List.map ~f:(fun o -> (head, o)) rest) @ result)
  in
  fn list []

let compare_all_projects projects =
  let project_pairs = generate_pairs projects in
  let all_compare_result_thunk = List.map ~f:(fun (a, b) ->
     fun () -> compare_two_projects a b
  ) project_pairs in
  all_compare_result_thunk

let top_k_file_compare_results project_compare_result_thunks ~k =
    let cmp a b = (max a.project_a_file_line_matches a.project_b_file_line_matches) - (max b.project_a_file_line_matches b.project_b_file_line_matches) in
    let heap = Pairing_heap.create ~cmp:cmp () in
    List.iter project_compare_result_thunks ~f:(fun thunk ->
        let project_compare_result = thunk () in
        let file_compare_results = project_compare_result.file_compare_results in
        List.iter file_compare_results ~f:(fun file_compare_result ->
            if ((Pairing_heap.length heap) < k) then begin
                Pairing_heap.add heap file_compare_result;
            end else if (cmp file_compare_result (Pairing_heap.top_exn heap)) > 0 then begin
                let _ : file_compare_result = Pairing_heap.pop_exn heap in
                Pairing_heap.add heap file_compare_result;
            end;
            
        );
    );
    List.rev (List.sort (Pairing_heap.to_list heap) ~compare:cmp)

let build_projects projects_parent_dir ~k ~w =
  let project_dirs = list_folders projects_parent_dir in
  let projects = List.map ~f:(build_project ~k:k ~w:w) project_dirs in
  projects

