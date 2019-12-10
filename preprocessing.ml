
(* Refer to preprocessing.mli for this function's speficiations *)
let k_grams s k =
  let rec k_grams_helper acc s n =
    try
      let n_sub_str = String.sub s 0 n in
      let tail_str = String.sub s 1 ((String.length s)-1) in
      k_grams_helper (n_sub_str::acc) tail_str n
    with Invalid_argument _ -> List.rev acc
  in
  k_grams_helper [] s k

let read_file f =
  let rec hash_helper f_channel s =
      try
        let line = input_line f_channel in
        hash_helper f_channel (s^line^"\n")
      with
      | End_of_file ->
        close_in f_channel;
        s
  in
  hash_helper (open_in f) ""

type project_file =
  {
    file_name: string;
    file_content: string;
  }

let process_file file_name =
  { file_name; file_content = read_file file_name; }
  
(* Refer to preprocessing.mli for this function's speficiations *)
let hash_file processed_file =
  let n_grams = k_grams processed_file.file_content 35 in
  List.map (Hashtbl.hash) n_grams

(* Refer to preprocessing.mli for this function's speficiations *)
let get_file_positions processed_file positions =
  let n_grams = k_grams processed_file.file_content 35 in
  List.map (fun p ->
    (p, List.nth n_grams (p - 1))
  ) positions

