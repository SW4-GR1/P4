open OUnit2
open Frontend 
open Frontend.Ast
open Test_utils.Mkast
open Test_utils.Strutils
open Test_utils.Typecheckerutils

let test_typechecker_unop_inc_on_int test_ctxt =
  let ast = mk_expr (EUnop ("x", Inc)) in
  let vtab = SymTab.bind "x" Ttree.Tint mk_vtab in
  let ftab = mk_ftab in
  let (ty, expr) = Typechecker.checkExp ftab vtab ast in
  let expected_out = "(INT(x++))" in
  let ttree_string = remove_whitespace (Pp_type.pp_expr expr) in
  assert_equal expected_out ttree_string

let test_typechecker_unop_dec_on_longint test_ctxt =
  let ast = mk_expr (EUnop ("y", Dec)) in
  let vtab = SymTab.bind "y" Ttree.Tlongint mk_vtab in
  let ftab = mk_ftab in
  let (ty, expr) = Typechecker.checkExp ftab vtab ast in
  let expected_out = "(L_INT(y--))" in
  let ttree_string = remove_whitespace (Pp_type.pp_expr expr) in
  assert_equal expected_out ttree_string


let test_typechecker_unop_inc_on_bool test_ctxt =
  let ast = mk_expr (EUnop ("z", Inc)) in
  let vtab = SymTab.bind "z" Ttree.Tbool mk_vtab in
  let ftab = mk_ftab in
  let typecheck = fun () -> let _ = Typechecker.checkExp ftab vtab ast in ()
  in assert_raises (mk_error "++ operator applied to a non-numeric type") typecheck

let test_typechecker_unop_on_undeclared_var test_ctxt =
  let ast = mk_expr (EUnop ("a", Inc)) in
  let vtab = mk_vtab in
  let ftab = mk_ftab in
  let typecheck = fun () -> let _ = Typechecker.checkExp ftab vtab ast in ()
  in assert_raises (mk_error "Variable a has not been declared") typecheck

let unop_tests = "UnopTests" >::: [
  "test_typechecker_unop_inc_on_int" >:: test_typechecker_unop_inc_on_int;
  "test_typechecker_unop_dec_on_longint" >:: test_typechecker_unop_dec_on_longint;
  "test_typechecker_unop_inc_on_bool" >:: test_typechecker_unop_inc_on_bool;
  "test_typechecker_unop_on_undeclared_var" >:: test_typechecker_unop_on_undeclared_var;
]
