open OUnit2
open Frontend 
open Frontend.Ast
open Test_utils.Mkast
open Test_utils.Strutils
open Test_utils.Typecheckerutils


let test_typechecker_gdecl_valid _ =
  let gvdec = (mk_gvdec Int_ty (mk_ident "x") (mk_expr (EConst 2))) in
  let ast = mk_stmt (Sglobal_var gvdec) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let (_, _, typechecked_ast) = Typechecker.checkStmt ftab vtab ast in
  let expected_out = "globalINTx=(INT2)" in
  let ttree_string = remove_whitespace (Pp_type.pp_stmt typechecked_ast) in
  assert_equal ~printer:(fun x -> x) expected_out ttree_string

let test_typechecker_gdecl_duplicate _ =
  let gvdec = (mk_gvdec Int_ty (mk_ident "x") (mk_expr (EConst 2))) in
  let gvdec2 = (mk_gvdec Int_ty (mk_ident "x") (mk_expr (EConst 22))) in
  let ast = mk_stmt (Slist [mk_stmt (Sglobal_var gvdec); mk_stmt (Sglobal_var gvdec)]) in
  let ftab = mk_ftab in
  let vtab = mk_ftab in
  let typecheck = fun () -> let _ = Typechecker.checkStmt ftab vtab ast in ()
  in assert_raises (mk_error "Duplicate declaration of: x") typecheck

let test_typechecker_gdecl_incompatible _ =
  let gvdec = (mk_gvdec Int_ty (mk_ident "x") (mk_expr (EBool true))) in
  let ast = mk_stmt (Sglobal_var gvdec) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let typecheck = fun () -> let _ = Typechecker.checkStmt ftab vtab ast in ()
  in assert_raises (mk_error "Incompatible types int and bool") typecheck


let gvardec_tests = "gvardecTests" >::: [
  "test_typechecker_gdecl_valid" >:: test_typechecker_gdecl_valid;
  "test_typechecker_gdecl_duplicate" >:: test_typechecker_gdecl_duplicate;
  "test_typechecker_gdecl_incompatible" >:: test_typechecker_gdecl_incompatible;
 
]