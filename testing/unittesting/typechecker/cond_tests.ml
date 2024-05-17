open OUnit2
open Helper 
open Frontend 
open Frontend.Ast

(* Tests for conditional expressions *)

let test_typechecker_cond_eq_integers test_ctxt =
  let ast = mk_expr (ECond (Eq, mk_expr (EConst 5), mk_expr (EConst 5))) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let (ty, expr) = Typechecker.checkExp ftab vtab ast in
  let expected_out = "(BOOL((INT5)==(INT5)))" in
  let ttree_string = remove_whitespace (Pp_type.pp_expr expr) in
  assert_equal expected_out ttree_string 

let test_typechecker_cond_neq_bools test_ctxt =
  let ast = mk_expr (ECond (Neq, mk_expr (EBool true), mk_expr (EBool false))) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let (ty, expr) = Typechecker.checkExp ftab vtab ast in
  let expected_out = "(BOOL((BOOLtrue)!=(BOOLfalse)))" in
  let ttree_string = remove_whitespace (Pp_type.pp_expr expr) in
  assert_equal expected_out ttree_string

let test_typechecker_cond_incompatible_types test_ctxt =
  let ast = mk_expr (ECond (Eq, mk_expr (EConst 10), mk_expr (EBool true))) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let typecheck = fun () -> let _ = Typechecker.checkExp ftab vtab ast in ()
  in assert_raises (mk_error "Invalid operation on non-numeric types") typecheck

let test_typechecker_cond_invalid_operation test_ctxt =
  let ast = mk_expr (ECond (Ast.Gt, mk_expr (EBool true), mk_expr (EBool false))) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let typecheck = fun () -> let _ = Typechecker.checkExp ftab vtab ast in ()
  in assert_raises (mk_error "Invalid operation on non-numeric types") typecheck

let cond_tests = "CondTests" >::: [
  "test_typechecker_cond_eq_integers" >:: test_typechecker_cond_eq_integers;
  "test_typechecker_cond_neq_bools" >:: test_typechecker_cond_neq_bools;
  "test_typechecker_cond_incompatible_types" >:: test_typechecker_cond_incompatible_types;
  "test_typechecker_cond_invalid_operation" >:: test_typechecker_cond_invalid_operation;
]
