open OUnit2
open Frontend 
open Frontend.Ast
open Frontend.Ttree
open Test_utils.Mkast
open Test_utils.Strutils
open Test_utils.Typecheckerutils

let test_typechecker_return_outside_function test_ctxt = 
  let ast = mk_stmt (Sreturn(mk_expr (EConst 0))) in
  let ftab = mk_ftab in 
  let vtab = mk_vtab in
  let typecheck = fun () -> let _ = Typechecker.checkStmt ftab vtab ast in ()
  in assert_raises (mk_error "Return statement outside of function") typecheck

let return_tests = "returnTests" >::: [
  "test_typechecker_return_outside_function" >:: test_typechecker_return_outside_function;
]
