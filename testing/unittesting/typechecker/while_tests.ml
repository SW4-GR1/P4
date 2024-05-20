open OUnit2
open Frontend 
open Frontend.Ast
open Test_utils.Mkast
open Test_utils.Strutils
open Test_utils.Typecheckerutils


let test_typechecker_while_condition_non_boolean _ =
  let ast = mk_stmt (Swhile(mk_expr (EConst 69), mk_stmt (Ssimple (mk_expr (EConst 42))))) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let typecheck = fun () -> let _ = Typechecker.checkStmt ftab vtab ast in ()
  in assert_raises (mk_error "Condition must evaluate to a boolean value") typecheck

let test_typechecker_while_stmt_valid _ = 
  let ast = mk_stmt (Swhile(mk_expr (EBool true), mk_stmt (Ssimple (mk_expr (EConst 42))))) in
  let ftab = mk_ftab in 
  let vtab = mk_vtab in
  let (_, _, typechecked_ast) = Typechecker.checkStmt ftab vtab ast in
  let expected_out = "while((BOOLtrue)){(INT42)}" in
  let ttree_string = remove_whitespace (Pp_type.pp_stmt typechecked_ast) in
  print_endline ("Expected: " ^ expected_out);
  print_endline ("Actual: " ^ ttree_string);
  assert_equal expected_out ttree_string

let test_typechecker_while_stmt_valid_complex_condition _ = 
  let ast = mk_stmt (Swhile(mk_expr (ECond (Gt, mk_expr (EConst 2), mk_expr(EConst 1)) ), mk_stmt (Ssimple (mk_expr (EConst 42))))) in
  let ftab = mk_ftab in 
  let vtab = mk_vtab in
  let (_, _, typechecked_ast) = Typechecker.checkStmt ftab vtab ast in
  let expected_out = "while((BOOL((INT2)>(INT1)))){(INT42)}" in
  let ttree_string = remove_whitespace (Pp_type.pp_stmt typechecked_ast) in
  print_endline ("Expected: " ^ expected_out);
  print_endline ("Actual: " ^ ttree_string);
  assert_equal expected_out ttree_string
let while_tests = "whileTests" >::: [
  "test_typechecker_while_condition_non_boolean" >:: test_typechecker_while_condition_non_boolean;
  "test_typechecker_while_stmt_valid " >:: test_typechecker_while_stmt_valid;
  "test_typechecker_while_stmt_valid_2 " >:: test_typechecker_while_stmt_valid_complex_condition;

]