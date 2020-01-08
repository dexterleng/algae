open Core


type kgram = {
  length: int;
  lines_occupied: int;
  (* zero-indexed; line 1 is 0 *)
  starting_line: int;
  starting_index_in_line: int;
  hash: int;
} [@@deriving yojson]

let rec list_zip list_a list_b =
  match list_a, list_b with
    | head_a::rest_a, head_b::rest_b -> (head_a, head_b)::(list_zip rest_a rest_b)
    | _, _ -> []

type document = Char of char | Newline

let convert_to_document lines = 
    List.concat_map 
        ~f:(fun line -> 
            let char_list = String.to_list line in 
            let rev_chars = List.map ~f:(fun c -> Char c)  (List.rev char_list) in
            List.rev (Newline::rev_chars)
        )
        lines

let take_ignoring_newline doc n = 
    let rec fn doc chars char_count =
        if char_count = n then 
            String.of_char_list (List.rev chars)
        else match doc with
        | (Char c) :: rest -> fn rest (c::chars) (char_count + 1)
        | Newline :: rest -> fn rest chars char_count
        | [] -> String.of_char_list (List.rev chars)
    in
    fn doc [] 0


let count_newline_in_window doc n =
    let rec fn doc n char_count newline_count =
        if char_count = n then newline_count
        else
            match doc with
            | Char _ :: rest -> fn rest n (char_count + 1) newline_count
            | Newline :: rest -> fn rest n char_count (newline_count + 1)
            | [] -> failwith "n > no. of chars"
    in
    fn doc n 0 0

let generate_n_gram_from_document doc n =
    let rec fn doc n curr_line curr_index =
        match doc with
        | Char(_)::rest -> 
            (try
                let kgram_chars = take_ignoring_newline doc n in
                let lines_occupied = (count_newline_in_window doc n) + 1 in
                let hash = Hashtbl.hash kgram_chars in
                let ngram = { length = n; lines_occupied; hash; starting_line = curr_line; starting_index_in_line = curr_index; } in
                ngram::(fn rest n curr_line (curr_index + 1))
            with | Failure _ -> [])
        | Newline::rest -> fn rest n (curr_line + 1) 0
        | [] -> []
    in
    fn doc n 0 0
