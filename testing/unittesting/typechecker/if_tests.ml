open OUnit2
open Frontend 
open Frontend.Ast
open Frontend.Ttree
open Test_utils.Mkast
open Test_utils.Strutils
open Test_utils.Typecheckerutils


let test_typechecker_if_stmt_condition_is_not_boolean test_ctxt = 
  let ast = mk_stmt (Sif(mk_expr (EConst 69), mk_stmt (Ssimple (mk_expr (EConst 42))), mk_stmt (Ssimple (mk_expr (EConst 0))))) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let typecheck = fun () -> let _ = Typechecker.checkStmt ftab vtab ast in ()
  in assert_raises (mk_error "Condition must evaluate to a boolean value") typecheck

let test_typechecker_if_stmt_condition_is_boolean test_ctxt = 
  let ast = mk_stmt (Sif(mk_expr (EBool true), mk_stmt (Ssimple (mk_expr (EConst 42))), mk_stmt (Ssimple (mk_expr (EConst 0))))) in
  let ftab = mk_ftab in 
  let vtab = mk_vtab in
  let (_, _, typechecked_ast) = Typechecker.checkStmt ftab vtab ast in
  let expected_out = "if((BOOLtrue)){(INT42)}else{(INT0)}" in
  let ttree_string = remove_whitespace (Pp_type.pp_stmt typechecked_ast) in
  assert_equal expected_out ttree_string
let test_typechecker_if_stmt_condition_is_boolean_no_else test_ctxt = 
  let ast = mk_stmt (Sif(mk_expr (EBool true), mk_stmt (Ssimple (mk_expr (EConst 42))), mk_stmt (Slist []))) in
  let ftab = mk_ftab in 
  let vtab = mk_vtab in
  let (_, _, typechecked_ast) = Typechecker.checkStmt ftab vtab ast in
  let expected_out = "if((BOOLtrue)){(INT42)}" in
  let ttree_string = remove_whitespace (Pp_type.pp_stmt typechecked_ast) in
  print_endline ("Expected: " ^ expected_out);
  print_endline ("Actual: " ^ ttree_string);
  assert_equal expected_out ttree_string

let if_tests = "ifTests" >::: [
  "test_typechecker_if_stmt_condition_is_not_boolean" >:: test_typechecker_if_stmt_condition_is_not_boolean;
  "test_typechecker_if_stmt_condition_is_boolean" >:: test_typechecker_if_stmt_condition_is_boolean; 
  "let test_typechecker_if_stmt_condition_is_boolean_no_else" >:: test_typechecker_if_stmt_condition_is_boolean_no_else
]