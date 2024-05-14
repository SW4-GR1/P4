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
  


  let fcall_tests = "FcallTests" >::: [
    "test_typechecker_fcall_argument_mismatch_to_many_arguments" >:: test_typechecker_fcall_argument_mismatch_to_many_arguments;
    "test_typechecker_fcall_argument_mismatch_to_few_arguments" >:: test_typechecker_fcall_argument_mismatch_to_few_arguments;
    "test_typechecker_fcall_argument_type_mismatch" >:: test_typechecker_fcall_argument_type_mismatch;  
    (* Add more tests here *)
  ]