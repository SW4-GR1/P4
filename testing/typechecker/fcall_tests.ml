open OUnit2
open Helper 
open Frontend 
open Frontend.Ast

(* (ty * ty list) list *)
let test_typechecker_fcall_argument_mismatch_to_many_arguments _test_ctxt =
  let ast = mk_expr (EFcall (mk_ident "f", [mk_expr (EConst 1); mk_expr (EConst 2)])) in
  let ftab = SymTab.bind "f" (Ttree.Tint,  [Ttree.Tint]) mk_ftab in
  let vtab = mk_vtab in
  let typecheck = fun () -> let _ = Typechecker.checkExp ftab vtab ast in ()
  in assert_raises (mk_error "Expected 1 argument(s), but got 2") typecheck


let test_typechecker_fcall_argument_mismatch_to_few_arguments _test_ctxt =
  let ast = mk_expr (EFcall (mk_ident "f", [mk_expr (EConst 1)])) in
  let ftab = SymTab.bind "f" (Ttree.Tint,  [Ttree.Tint; Ttree.Tint]) mk_ftab in
  let vtab = mk_vtab in
  let typecheck = fun () -> let _ = Typechecker.checkExp ftab vtab ast in ()
  in assert_raises (mk_error "Expected 2 argument(s), but got 1") typecheck


let test_typechecker_fcall_argument_type_mismatch _test_ctxt =
  let ast = mk_expr (EFcall (mk_ident "f", [mk_expr (EConst 1); mk_expr (EFloat 2.1)])) in
  let ftab = SymTab.bind "f" (Ttree.Tint,  [Ttree.Tint; Ttree.Tint]) mk_ftab in
  let vtab = mk_vtab in
  let typecheck = fun () -> let _ = Typechecker.checkExp ftab vtab ast in ()
  in assert_raises (mk_error "Type of arguments in function call do not match") typecheck
  
let test_typechecker_fcall_correct_call_2_arguments_2_integers _test_ctxt =
  let ast = mk_expr (EFcall (mk_ident "f", [mk_expr (EConst 1); mk_expr (EConst 2)])) in
  let ftab = SymTab.bind "f" (Ttree.Tint, [Ttree.Tint; Ttree.Tint]) mk_ftab in
  let vtab = mk_vtab in
  let vtab' = vtab in
  let (ty, expr) = Typechecker.checkExp ftab vtab' ast in
  let expected_out = "f((INT1),(INT2))" in
  let ttree_string = remove_whitespace (Pp_type.pp_expr expr) in
  assert_equal expected_out ttree_string

let test_typechecker_fcall_correct_call_2_arguments_1_integer_1_bool _test_ctxt =
  let ast = mk_expr (EFcall (mk_ident "f", [mk_expr (EConst 1); mk_expr (EBool true)])) in
  let ftab = SymTab.bind "f" (Ttree.Tint, [Ttree.Tint; Ttree.Tbool]) mk_ftab in
  let vtab = mk_vtab in
  let vtab' = vtab in
  let (ty, expr) = Typechecker.checkExp ftab vtab' ast in
  let expected_out = "f((INT1),(BOOLtrue))" in
  let ttree_string = remove_whitespace (Pp_type.pp_expr expr) in
  assert_equal expected_out ttree_string

let test_typechecker_fcall_correct_call_1_argument_1_float _test_ctxt =
  let ast = mk_expr (EFcall (mk_ident "f", [mk_expr (EFloat 1.1)])) in
  let ftab = SymTab.bind "f" (Ttree.Tint, [Ttree.Tfloat]) mk_ftab in
  let vtab = mk_vtab in
  let vtab' = vtab in
  let (ty, expr) = Typechecker.checkExp ftab vtab' ast in
  let expected_out = "f((FLOAT1.1))" in
  let ttree_string = remove_whitespace (Pp_type.pp_expr expr) in
  assert_equal expected_out ttree_string

  let fcall_tests = "FcallTests" >::: [
    "test_typechecker_fcall_argument_mismatch_to_many_arguments" >:: test_typechecker_fcall_argument_mismatch_to_many_arguments;
    "test_typechecker_fcall_argument_mismatch_to_few_arguments" >:: test_typechecker_fcall_argument_mismatch_to_few_arguments;
    "test_typechecker_fcall_argument_type_mismatch" >:: test_typechecker_fcall_argument_type_mismatch;  
    "test_typechecker_fcall_correct_call_2_arguments_2_integers" >:: test_typechecker_fcall_correct_call_2_arguments_2_integers; 
    "test_typechecker_fcall_correct_call_2_arguments_1_integer_1_bool" >:: test_typechecker_fcall_correct_call_2_arguments_1_integer_1_bool; 
    "test_typechecker_fcall_correct_call_1_argument_1_float" >:: test_typechecker_fcall_correct_call_1_argument_1_float; 
    (* Add more tests here *)
  ]