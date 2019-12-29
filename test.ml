open OUnit2

let suite = "Final Project Test Suite" >:::
  Test_preprocessing.tests @ Test_winnow.tests 

(* The following line must be the one and only place
 * in your entire source code that calls [OUnit2.run_test_tt_main]. *)
let () = run_test_tt_main suite
