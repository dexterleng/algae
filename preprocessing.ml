open Core

let rec generate_n_grams n l =
  if n <= List.length l
    then (List.take l n)::(generate_n_grams n (List.drop l 1))
  else
    []

let yojson_of_int_set s = Int.Set.to_list s

type kgram = {
  length: int;
  (* line number is zero indexed *)
  occupying_lines: int list;
  (* this is just min of occupying_lines *)
  starting_line: int;
  starting_index_in_line: int;
  hash: int;
} [@@deriving yojson]

let rec list_zip list_a list_b =
  match list_a, list_b with
    | head_a::rest_a, head_b::rest_b -> (head_a, head_b)::(list_zip rest_a rest_b)
    | _, _ -> []

let k_grams_with_line_number lines k =
  let chars_with_position = List.concat_mapi ~f:(fun line_index line ->
    let chars = List.mapi ~f:(fun char_index char -> (char, line_index, char_index)) (String.to_list line) in
    chars
  ) lines
  in
  let k_grams = generate_n_grams k chars_with_position in
  k_grams
    |> List.map ~f:(fun chars ->
      let length = k in
      let (_, starting_line, starting_index_in_line) = (List.nth_exn chars 0) in
      let kgram_string = String.of_char_list (List.map ~f:(fun (c,_,_) -> c) chars) in
      let hash = Hashtbl.hash kgram_string in
      let occupying_lines_repeating = List.map ~f: (fun (_,l,_) -> l) chars in
      let occupying_lines = List.fold_left
        ~init: Int.Set.empty
        ~f: Int.Set.add
        occupying_lines_repeating |> Int.Set.to_list in
      { length; occupying_lines; starting_line; starting_index_in_line; hash; }
    )