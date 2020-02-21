open OUnit2
open Core
open Project_compare
open Preprocessing

let kgram1 = {
    hash = 10;
    str = "abc";
    length = 20;
    lines_occupied = 1;
    starting_line = 1;
    starting_index_in_line = 5;
}

let kgram2 = {
    hash = 999;
    str = "123";
    length = 20;
    lines_occupied = 1;
    starting_line = 1;
    starting_index_in_line = 5;
}

let kgram3 = {
    hash = 123;
    str = "qwe";
    length = 20;
    lines_occupied = 1;
    starting_line = 1;
    starting_index_in_line = 5;
}

let tests = [
  "generate_range_set" >:: (fun _ -> assert_equal ~cmp:(Set.equal)
      (Set.of_list (module Int) [1;2;3;4;5;]) 
      (generate_range_set 1 5)
  );

  "calculate_match_density" >:: (fun _ -> assert_equal
    ((1.0, 1.0))
    (calculate_match_density (build_kgram_by_hash_map [kgram1; kgram1; kgram1;]) (build_kgram_by_hash_map [kgram1; kgram1; kgram1;]))
  );
  "calculate_match_density2" >:: (fun _ -> assert_equal
    ((1.0 /. 3.0, 1.0))
    (calculate_match_density (build_kgram_by_hash_map [kgram1; kgram2; kgram3;]) (build_kgram_by_hash_map [kgram1; kgram1; kgram1;]))
  );
  "calculate_match_density3" >:: (fun _ -> assert_equal
    ((0.0, 0.0))
    (calculate_match_density (build_kgram_by_hash_map [kgram1; kgram1; kgram1;]) (build_kgram_by_hash_map [kgram2; kgram2; kgram2;]))
  );
  "calculate_match_density4" >:: (fun _ -> assert_equal
    ((0.5, 2.0 /. 3.0))
    (calculate_match_density (build_kgram_by_hash_map [kgram1; kgram2;]) (build_kgram_by_hash_map [kgram2; kgram2; kgram3;]))
  );

  "calculate_line_matches" >:: (fun _ -> assert_equal
    (1, 1)
    (calculate_line_matches (build_kgram_by_hash_map [kgram1; kgram1;]) (build_kgram_by_hash_map [kgram1; kgram1;]))
  );
  "calculate_line_matches2" >:: (fun _ -> assert_equal
    (1, 10)
    (calculate_line_matches (build_kgram_by_hash_map [kgram3;]) (build_kgram_by_hash_map [{ kgram3 with lines_occupied = 10;  };]))
  );
  "calculate_line_matches3" >:: (fun _ -> assert_equal
    (1, 10)
    (calculate_line_matches (build_kgram_by_hash_map [kgram3;]) (build_kgram_by_hash_map [kgram3; { kgram3 with lines_occupied = 10 };]))
  );
  "calculate_line_matches4" >:: (fun _ -> assert_equal
    (1, 20)
    (calculate_line_matches (build_kgram_by_hash_map [kgram3;]) (build_kgram_by_hash_map [{ kgram3 with starting_line = 100; lines_occupied = 10; }; { kgram3 with lines_occupied = 10 };]))
  );
]
