open OUnit2
open Frontend 
open Frontend.Ast
open Test_utils.Mkast
open Test_utils.Strutils
open Test_utils.Typecheckerutils


let test_typechecker_for_loop_stmt_condition_non_bool _ = 
  let vdec = { var_ty = Int_ty; var_name = mk_ident "x"; var_expr = Some (mk_expr (EConst 2)) } in
  let ast = mk_stmt (Sfor(mk_stmt(Sdecl(vdec)), mk_expr (EConst 2), mk_stmt (Ssimple (mk_expr (EUnop("x", Inc)))), mk_stmt (Ssimple (mk_expr (EConst 42))))) in
  let ftab = mk_ftab in 
  let vtab = mk_vtab in
  let typecheck = fun () -> let _ = Typechecker.checkStmt ftab vtab ast in ()
  in assert_raises (mk_error "Condition must evaluate to a boolean value") typecheck

let test_typechecker_for_loop_stmt_valid _ = 
  let vdec = { var_ty = Int_ty; var_name = mk_ident "x"; var_expr = Some (mk_expr (EConst 2)) } in
  let ast = mk_stmt (Sfor(mk_stmt(Sdecl(vdec)), mk_expr (EBool true), mk_stmt (Ssimple (mk_expr (EUnop("x", Inc)))), mk_stmt (Ssimple (mk_expr (EConst 42))))) in
  let ftab = mk_ftab in 
  let vtab = mk_vtab in
  let (_, _, typechecked_ast) = Typechecker.checkStmt ftab vtab ast in
  let expected_out = "for(letINTx=(INT2);(BOOLtrue);(INT(x++))){(INT42)}" in
  let ttree_string = remove_whitespace (Pp_type.pp_stmt typechecked_ast) in
  assert_equal expected_out ttree_string


let test_typechecker_for_loop_stmt_valid_complex_condition _ = 
  let vdec = { var_ty = Int_ty; var_name = mk_ident "x"; var_expr = Some (mk_expr (EConst 2)) } in
  let ast = mk_stmt (Sfor(mk_stmt(Sdecl(vdec)), mk_expr (ECond (Gt, mk_expr (EConst 2), mk_expr(EConst 1)) ), mk_stmt (Ssimple (mk_expr (EUnop("x", Inc)))), mk_stmt (Ssimple (mk_expr (EConst 42))))) in
  let ftab = mk_ftab in 
  let vtab = mk_vtab in
  let (_, _, typechecked_ast) = Typechecker.checkStmt ftab vtab ast in
  let expected_out = "for(letINTx=(INT2);(BOOL((INT2)>(INT1)));(INT(x++))){(INT42)}" in
  let ttree_string = remove_whitespace (Pp_type.pp_stmt typechecked_ast) in
  assert_equal expected_out ttree_string
let for_loop_tests = "forloopTests" >::: [
  "test_typechecker_for_loop_stmt_valid_complex_condition" >:: test_typechecker_for_loop_stmt_valid_complex_condition;
  "test_typechecker_for_loop_stmt_valid" >:: test_typechecker_for_loop_stmt_valid;
  "test_typechecker_for_loop_stmt_condition_non_bool" >:: test_typechecker_for_loop_stmt_condition_non_bool;

]