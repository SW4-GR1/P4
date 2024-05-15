open OUnit2
open Helper 
open Frontend 
open Frontend.Ast


let test_typechecker_elog_and_operator_1_bool_1_integer test_ctxt =
  let ast = mk_expr (ELog (And, mk_expr (EBool false), mk_expr (EConst 2))) in
  let ftab = mk_ftab in 
  let vtab = mk_vtab in
  let typecheck = fun () -> let _ = Typechecker.checkExp ftab vtab ast in ()
  in assert_raises (mk_error "operator applied to a non-boolean type") typecheck

let test_typechecker_elog_or_operator_1_bool_1_integer test_ctxt =
  let ast = mk_expr (ELog (Or, mk_expr (EBool true), mk_expr (EConst 2))) in
  let ftab = mk_ftab in 
  let vtab = mk_vtab in
  let typecheck = fun () -> let _ = Typechecker.checkExp ftab vtab ast in ()
  in assert_raises (mk_error "operator applied to a non-boolean type") typecheck

let log_tests = "LogTests" >::: [
  "test_typechecker_elog_and_operator_1_bool_1_integer" >:: test_typechecker_elog_and_operator_1_bool_1_integer;
  "test_typechecker_elog_or_operator_1_bool_1_integer" >:: test_typechecker_elog_or_operator_1_bool_1_integer;
]