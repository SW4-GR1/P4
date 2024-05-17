open OUnit2
open Typechecker_testing


let suite = "TypecheckerTests" >::: [
  Datatype_tests.datatype_tests;
  Binop_tests.binop_tests;
  Cond_tests.cond_tests;
  Fcall_tests.fcall_tests;
  Log_tests.log_tests;
  Unop_tests.unop_tests;
  Not_tests.not_tests;
  If_tests.if_tests;
  Fdec_tests.fdec_tests;
    (* Add more tests here *)
] 
  
(* let _ = run_test_tt_main suite 
 *)