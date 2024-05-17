open OUnit2
open Frontend 
open Frontend.Ast
open Test_utils.Mkast
open Test_utils.Strutils
open Test_utils.Typecheckerutils

(* Tests for the not operator in conditional expressions *)

let test_typechecker_not_on_bool test_ctxt =
  let ast = mk_expr (ENot (mk_expr (EBool true))) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let (ty, expr) = Typechecker.checkExp ftab vtab ast in
  let expected_out = "(BOOL(!(BOOLtrue)))" in
  let ttree_string = remove_whitespace (Pp_type.pp_expr expr) in
  assert_equal expected_out ttree_string

let test_typechecker_not_on_int test_ctxt =
  let ast = mk_expr (ENot (mk_expr (EConst 10))) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let typecheck = fun () -> let _ = Typechecker.checkExp ftab vtab ast in ()
  in assert_raises (mk_error "Not operator applied to a non-boolean type") typecheck

let not_tests = "NotTests" >::: [
  "test_typechecker_not_on_bool" >:: test_typechecker_not_on_bool;
  "test_typechecker_not_on_int" >:: test_typechecker_not_on_int;
]
