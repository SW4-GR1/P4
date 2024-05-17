open OUnit2
open Frontend
open Frontend.Ast
open Test_utils.Mkast
open Test_utils.Strutils
open Test_utils.Typecheckerutils

let test_typecheck_binop_add_const _test_ctxt = 
  (* Create an AST representing the expression "1 + 1" *)
  let ast = mk_expr (EBinop (Add, mk_expr (EConst 1), mk_expr (EConst 1))) in
  let expected_out = "(INT((INT1)+(INT1)))" in
  (* Typecheck the AST *)
  let ftab = mk_ftab in 
  let vtab = mk_vtab in
  let (ty, expr) = Typechecker.checkExp ftab vtab ast in
  let ttree_string = remove_whitespace (Pp_type.pp_expr expr) in
  print_endline ttree_string;
  assert_equal Ttree.Tint ty;
  assert_equal expected_out ttree_string

let test_typecheck_binop_add_const_and_float _test_ctxt =
  (* Create an AST representing the expression "1 + 1.0" *)
  let ast = mk_expr (EBinop (Add, mk_expr (EConst 1), mk_expr (EFloat 1.0))) in
  (* Typecheck the AST *)
  let ftab = mk_ftab in 
  let vtab = mk_vtab in
  let typecheck = fun () -> let _ = Typechecker.checkExp ftab vtab ast in ()
  in
  assert_raises (mk_error "Incompatible types int and float") typecheck
  
let test_typecheck_binop_add_const_and_bool _test_ctxt =
  (* Create an AST representing the expression "1 + true" *)
  let ast = mk_expr (EBinop (Add, mk_expr (EConst 1), mk_expr (EBool true))) in
  (* Typecheck the AST *)
  let ftab = mk_ftab in 
  let vtab = mk_vtab in
  let typecheck = fun () -> let _ = Typechecker.checkExp ftab vtab ast in ()
  in
  assert_raises (mk_error "Binary operator should only be used on numbers") typecheck

let test_typecheck_binop_mod_float _test_ctxt =
  let ast = mk_expr (EBinop (Mod, mk_expr (EConst 1), mk_expr (EFloat 1.0))) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let typecheck = fun () -> let _ = Typechecker.checkExp ftab vtab ast in ()
  in assert_raises (mk_error "Modulo operator should only be used on integers") typecheck

let test_typecheck_binop_div_0 _test_ctxt =
  let ast = mk_expr (EBinop (Div, mk_expr (EConst 1), mk_expr (EConst 0))) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let typecheck = fun () -> let _ = Typechecker.checkExp ftab vtab ast in ()
  in assert_raises (mk_error "Division by 0 not allowed") typecheck


(* In a perfect world the compiler raises an error if divisor evals to 0*)
let test_typechecker_binop_div_eval0 _test_ctxt =
  let ast = mk_expr (EBinop (Div, mk_expr (EConst 1), mk_expr (EBinop (Sub, mk_expr (EConst 1), mk_expr (EConst 1))))) in
  let expected_out = "(INT((INT1)/(INT((INT1)-(INT1)))))" in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let (_, expr) = Typechecker.checkExp ftab vtab ast in 
  let ttree_string = remove_whitespace (Pp_type.pp_expr expr) in
  assert_equal expected_out ttree_string

let test_typechecker_binop_add_int_and_ident_of_longint _test_ctxt =
  let ast = mk_expr (EBinop (Add, mk_expr (EConst 1), mk_expr (EIdent (mk_ident "x")))) in
  let expected_out = "(L_INT((INT1)+(L_INTx)))" in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let vtab' = SymTab.bind "x" Ttree.Tlongint vtab in
  let (ty, expr) = Typechecker.checkExp ftab vtab' ast in
  let ttree_string = remove_whitespace (Pp_type.pp_expr expr) in
  assert_equal Ttree.Tlongint ty;
  assert_equal expected_out ttree_string

let test_typechecker_binop_add_float_and_ident_of_longfloat _test_ctxt =
  let ast = mk_expr (EBinop (Add, mk_expr (EFloat 1.1), mk_expr (EIdent (mk_ident "x")))) in
  let expected_out = "(L_FLOAT((FLOAT1.1)+(L_FLOATx)))" in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let vtab' = SymTab.bind "x" Ttree.Tlongfloat vtab in
  let (ty, expr) = Typechecker.checkExp ftab vtab' ast in
  let ttree_string = remove_whitespace (Pp_type.pp_expr expr) in
  assert_equal Ttree.Tlongfloat ty;
  assert_equal expected_out ttree_string

let binop_tests = "BinopTests" >::: [
  "test_typecheck:add 1+1" >:: test_typecheck_binop_add_const;
  "test_typecheck:add 1+1.0_incompatible_types" >:: test_typecheck_binop_add_const_and_float;
  "test_typecheck:add 1+true :raises error" >:: test_typecheck_binop_add_const_and_bool;
  "test_typecheck:mod 1%1.0_raises error" >:: test_typecheck_binop_mod_float;
  "test_typecheck:div 1/0_raises error" >:: test_typecheck_binop_div_0;
  "test_typecheck:div 1/(1-1)_raises error" >:: test_typechecker_binop_div_eval0;
  "test_typecheck:add 1+x" >:: test_typechecker_binop_add_int_and_ident_of_longint;
  "test_typecheck:add 1.1+x" >:: test_typechecker_binop_add_float_and_ident_of_longfloat;
  (* Add more tests here *)
]