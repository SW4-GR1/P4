open OUnit2
open Frontend 
open Frontend.Ast
open Test_utils.Mkast
open Test_utils.Strutils
open Test_utils.Typecheckerutils


let test_typechecker_assign_valid _ =
  let vdec = { var_ty = Int_ty; var_name = mk_ident "x"; var_expr = None } in
  let ast = mk_stmt(Slist[mk_stmt((Sdecl vdec)) ;mk_stmt (Sass( "x", mk_assign_type "Assign", mk_expr(EConst 3)) )]) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let (_, _, typechecked_ast) = Typechecker.checkStmt ftab vtab ast in
  let expected_out = "letINTx" ^"x=(INT3)" in
  let ttree_string = remove_whitespace (Pp_type.pp_stmt typechecked_ast) in
  print_endline ("Expected: " ^ expected_out);
  print_endline ("Actual: " ^ ttree_string);
  assert_equal expected_out ttree_string

let vassign_tests = "vassignTests" >::: [
  "test_typechecker_assign_valid" >:: test_typechecker_assign_valid;
]
