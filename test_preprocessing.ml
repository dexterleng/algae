open OUnit2
open Preprocessing
open Core

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
    [
      { 
        length = 5;
        occupying_lines = [0;1];
        starting_line = 0;
        starting_index_in_line = 0;
        hash = Hashtbl.hash "hithe";
      };
      { 
        length = 5;
        occupying_lines = [0;1];
        starting_line = 0;
        starting_index_in_line = 1;
        hash = Hashtbl.hash "ither";
      };
      { 
        length = 5;
        occupying_lines = [1];
        starting_line = 1;
        starting_index_in_line = 0;
        hash = Hashtbl.hash "there";
      };
      { 
        length = 5;
        occupying_lines = [1;2;];
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
