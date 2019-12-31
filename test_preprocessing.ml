open OUnit2
open Preprocessing
open Core

let doc1 = [
    Char 'a'; Newline; Char 'b'; Char 'c'; Char 'd'
]

let doc2 = [
    Char 'a'; Newline; Char 'b'; Newline; Char 'c'; Char 'd'
]


let tests = [
  "take_ignoring_newline" >:: (fun _ -> assert_equal
  "abc"
  (take_ignoring_newline doc1 3)
  );

  "take_ignoring_newline returns all chars when n > no. of chars" >:: (fun _ -> assert_equal
  "abcd" 
  (take_ignoring_newline doc1 9)
  );

  "count_newline_in_window" >:: (fun _ -> assert_equal
    2
    (count_newline_in_window doc2 4)
  );

  "count_newline_in_window fails when n > no. of chars" >:: (fun _ -> assert_raises
    (Failure "n > no. of chars") 
    (fun () -> count_newline_in_window doc2 5)
  );

  "generate_n_gram_from_document" >:: (fun _ -> assert_equal
  [
      { hash = Hashtbl.hash "ab"; length = 2; starting_line = 0; lines_occupied = 2 };
      { hash = Hashtbl.hash "bc"; length = 2; starting_line = 1; lines_occupied = 1 };
      { hash = Hashtbl.hash "cd"; length = 2; starting_line = 1; lines_occupied = 1 };
  ]
  (generate_n_gram_from_document doc1 2)
  );
]
