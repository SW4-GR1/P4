open OUnit2
open Frontend 
open Frontend.Ast
open Test_utils.Mkast
open Test_utils.Strutils
open Test_utils.Typecheckerutils


let test_typechecker_decl_valid test_ctxt =
  let vdec = { var_ty = Int_ty; var_name = mk_ident "x"; var_expr = Some (mk_expr (EConst 2)) } in
  let ast = mk_stmt (Sdecl vdec) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let (_, _, typechecked_ast) = Typechecker.checkStmt ftab vtab ast in
  let expected_out = "letINTx=(INT2)" in
  let ttree_string = remove_whitespace (Pp_type.pp_stmt typechecked_ast) in
  print_endline ("Expected: " ^ expected_out);
  print_endline ("Actual: " ^ ttree_string);
  assert_equal expected_out ttree_string

let test_typechecker_decl_duplicate _ =
  let vdec = { var_ty = Int_ty; var_name = mk_ident "x"; var_expr = Some (mk_expr (EConst 2)) } in
  let vdec2 = { var_ty = Int_ty; var_name = mk_ident "x"; var_expr = Some (mk_expr (EConst 22)) } in
  let ast = mk_stmt (Slist [mk_stmt (Sdecl vdec); mk_stmt (Sdecl vdec2)]) in
  let ftab = mk_ftab in
  let vtab = mk_ftab in
  let typecheck = fun () -> let _ = Typechecker.checkStmt ftab vtab ast in ()
  in assert_raises (mk_error "Duplicate declaration of: x") typecheck

let test_typechecker_decl_incompatible _ =
  let vdec = { var_ty = Int_ty; var_name = mk_ident "x"; var_expr = Some (mk_expr (EBool true)) } in
  let ast = mk_stmt (Sdecl vdec) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let typecheck = fun () -> let _ = Typechecker.checkStmt ftab vtab ast in ()
  in assert_raises (mk_error "Incompatible types int and bool") typecheck


let vardec_tests = "vardecTests" >::: [
  "test_typechecker_decl_valid" >:: test_typechecker_decl_valid;
  "test_typechecker_decl_duplicate" >:: test_typechecker_decl_duplicate;
  "test_typechecker_decl_incompatible" >:: test_typechecker_decl_incompatible
]
