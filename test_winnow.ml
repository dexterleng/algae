open OUnit2
open Winnowing
open Core

let res_to_string l = List.fold_left l ~init: "" ~f:(fun acc curr -> acc^(Int.to_string curr)^",")

(* tests to check Winnowing functionality *)
let tests = [
"winnow0" >:: (fun _ -> assert_equal ~printer:res_to_string [] (winnow [] ~k:1));
"winnow1" >:: (fun _ -> assert_equal ~printer:res_to_string [1;] (winnow [1] ~k:1));
"winnow2" >:: (fun _ -> assert_equal ~printer:res_to_string [17;17;8;39;17] (winnow [77;74;42;17;98;50;17;98;8;88;67;39;77;74;42;17;98] ~k:4));
"winnow3" >:: (fun _ -> assert_equal ~printer:res_to_string [] (winnow [77;74;42;17;98;] ~k:10));
]
