open OUnit2
open Typechecker_testing


let suite = "TypecheckerTests" >::: [
  Datatype_tests.datatype_tests;
  Binop_tests.binop_tests;
  Cond_tests.cond_tests;
  Fcall_tests.fcall_tests;
  Log_tests.log_tests;
    (* Add more tests here *)
] 
  
(* let _ = run_test_tt_main suite 
 *)