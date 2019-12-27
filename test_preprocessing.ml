open OUnit2
open Preprocessing
open Core

let a = 1

let build_int_set_from_list ints = List.fold_left
  ~init: Core.Int.Set.empty
  ~f: Core.Int.Set.add
  ints

let kgram_equals a b =
  (a.length = b.length) &&
  (Int.Set.equal a.occupying_lines b.occupying_lines) &&
  (a.starting_line = b.starting_line) &&
  (a.starting_index_in_line = b.starting_index_in_line) &&
  (a.hash = b.hash)

let rec kgram_list_equals a b =
  if (List.length a) <> (List.length b)
    then false
  else match (a, b) with
    | (kgram_a::rest_a, kgram_b::rest_b) -> (kgram_equals kgram_a kgram_b) && (kgram_list_equals rest_a rest_b)
    | (_,_) -> true

let tests = [
  "generate_n_grams works when n < length of array" >:: (fun _ -> assert_equal
    [
      [1;2];
      [2;3];
      [3;4];
    ]
    (generate_n_grams 2 [1;2;3;4])
  );
  "generate_n_grams returns one gram when n = length of array" >:: (fun _ -> assert_equal
    [
      [1;2;3;4;];
    ]
    (generate_n_grams 4 [1;2;3;4])
  );
  "generate_n_grams returns zero grams when n > length of array" >:: (fun _ -> assert_equal
    []
    (generate_n_grams 5 [1;2;3;4])
  );

  "list_zip zips two lists of same length into a tuple" >:: (fun _ -> assert_equal
    [
      (1,3);
      (2,4);
    ]
    (list_zip [1;2] [3;4])
  );
  "list_zip zips two lists of different length by ignoring the additional elements of the longer list" >:: (fun _ -> assert_equal
    [
      (1,3);
      (2,4);
    ]
    (list_zip [1;2;1000] [3;4])
  );

  "k_grams_with_line_number" >:: (fun _ -> assert_equal
    ~cmp: kgram_list_equals
    [
      { 
        length = 5;
        occupying_lines = (build_int_set_from_list [0;1]);
        starting_line = 0;
        starting_index_in_line = 0;
        hash = Hashtbl.hash "hithe";
      };
      { 
        length = 5;
        occupying_lines = (build_int_set_from_list [0;1]);
        starting_line = 0;
        starting_index_in_line = 1;
        hash = Hashtbl.hash "ither";
      };
      { 
        length = 5;
        occupying_lines = (build_int_set_from_list [1]);
        starting_line = 1;
        starting_index_in_line = 0;
        hash = Hashtbl.hash "there";
      };
      { 
        length = 5;
        occupying_lines = (build_int_set_from_list [1;2;]);
        starting_line = 1;
        starting_index_in_line = 1;
        hash = Hashtbl.hash "here.";
      };
    ]
    (k_grams_with_line_number [
      "hi";
      "there";
      ".";
    ] 5)
  )
]
