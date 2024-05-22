open OUnit2
open Frontend 
open Frontend.Ast
open Test_utils.Mkast
open Test_utils.Strutils
open Test_utils.Typecheckerutils


let test_typechecker_datatype_const _test_ctxt =
  let ast = mk_expr (EConst 1) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let (ty, expr) = Typechecker.checkExp ftab vtab ast in
  assert_equal Ttree.Tint ty;
  assert_equal "(INT1)" (remove_whitespace (Pp_type.pp_expr expr))

let test_typechecker_datatype_float _test_ctxt =
  let ast = mk_expr (EFloat 1.1) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let (ty, expr) = Typechecker.checkExp ftab vtab ast in
  assert_equal Ttree.Tfloat ty;
  assert_equal "(FLOAT1.1)" (remove_whitespace (Pp_type.pp_expr expr))

let test_typechecker_datatype_bool _test_ctxt =
  let ast = mk_expr (EBool true) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let (ty, expr) = Typechecker.checkExp ftab vtab ast in
  assert_equal Ttree.Tbool ty;
  assert_equal "(BOOLtrue)" (remove_whitespace (Pp_type.pp_expr expr))

let test_typechecker_datatype_ident_declared _test_ctxt =
  let ast = mk_expr (EIdent (mk_ident "x")) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in 
  let vtab' = SymTab.bind "x" Ttree.Tint vtab in
  let (ty, expr) = Typechecker.checkExp ftab vtab' ast in
  assert_equal Ttree.Tint ty;
  assert_equal "(INTx)" (remove_whitespace (Pp_type.pp_expr expr))

let test_typechecker_data_ident_unbound _test_ctxt =
  let ast = mk_expr (EIdent (mk_ident "x")) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let typecheck = fun () -> let _ = Typechecker.checkExp ftab vtab ast in ()
  in assert_raises (mk_error "Variable x has not been declared") typecheck

let datatype_tests = "DatatypeTests" >::: [
  "test_typecheck:bool" >:: test_typechecker_datatype_bool;
  "test_typecheck:int" >:: test_typechecker_datatype_const;
  "test_typecheck:float" >:: test_typechecker_datatype_float;
  "test_typecheck:ident_declared" >:: test_typechecker_datatype_ident_declared;
  "test_typecheck:ident_unbound" >:: test_typechecker_data_ident_unbound;
  (* Add more tests here *)
]