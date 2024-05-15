open OUnit2
open Helper 
open Frontend 
open Frontend.Ast

let test_typechecker_if_stmt_condition_is_not_boolean test_ctxt = 
  let ast = mk_stmt (Sif(mk_expr (EConst 69), mk_stmt (Ssimple (mk_expr (EConst 42))), mk_stmt (Ssimple (mk_expr (EConst 0))))) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let typecheck = fun () -> let _ = Typechecker.checkStmt ftab vtab ast in ()
  in assert_raises (mk_error "Condition must evaluate to a boolean value") typecheck


let if_tests = "ifTests" >::: [
  "test_typechecker_if_stmt_condition_is_not_boolean" >:: test_typechecker_if_stmt_condition_is_not_boolean;
]