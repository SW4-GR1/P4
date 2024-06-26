open OUnit2
open Frontend 
open Frontend.Ast
open Frontend.Ttree
open Test_utils.Mkast
open Test_utils.Strutils
open Test_utils.Typecheckerutils

let test_typechecker_func_two_args test_ctxt =
  let arg_dec = [(Int_ty, mk_ident "x"); (Bool_ty, mk_ident "y")]in 
  let fdec = {
    fun_type = Int_ty; 
    fun_name = mk_ident "f"; 
    args = arg_dec; 
    body = mk_stmt (Slist [mk_stmt (Ssimple (mk_expr (EConst 2))); mk_stmt (Sreturn (mk_expr (EConst 1)))]);
  } in
  let ast = mk_stmt (Sfunc fdec) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let (_, _, typechecked_ast) = Typechecker.checkStmt ftab vtab ast in
  let expected_out = "INTf(INTx,BOOLy){(INT2)return(INT1)}" in
  let ttree_string = remove_whitespace (Pp_type.pp_stmt typechecked_ast) in
  assert_equal expected_out ttree_string

let test_typechecker_func_duplicate_argname test_ctxt =
  let arg_dec = [(Int_ty, mk_ident "x"); (Bool_ty, mk_ident "x")]in 
  let fdec = {
    fun_type = Int_ty; 
    fun_name = mk_ident "f"; 
    args = arg_dec; 
    body = mk_stmt (Slist [mk_stmt (Ssimple (mk_expr (EConst 2))); mk_stmt (Sreturn (mk_expr (EConst 1)))]);
  } in
  let ast = mk_stmt (Sfunc fdec) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let typecheck = fun () -> let _ = Typechecker.checkStmt ftab vtab ast in () in 
  assert_raises (mk_error "Function argument x has been declared elsewhere") typecheck

let test_typechecker_func_no_args test_ctxt =
  let arg_dec = []in 
  let fdec = {
    fun_type = Int_ty; 
    fun_name = mk_ident "f"; 
    args = arg_dec; 
    body = mk_stmt (Slist [ mk_stmt (Sreturn (mk_expr (EConst 1)))]);
  } in
  let ast = mk_stmt (Sfunc fdec) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let (_, _, typechecked_ast) = Typechecker.checkStmt ftab vtab ast in
  let expected_out = "INTf(){return(INT1)}" in
  let ttree_string = remove_whitespace (Pp_type.pp_stmt typechecked_ast) in
  print_endline ("Expected: " ^ expected_out);
  print_endline ("Actual: " ^ ttree_string);
  assert_equal expected_out ttree_string

let test_typechecker_func_no_args_no_return test_ctxt =
  let arg_dec = []in 
  let fdec = {
    fun_type = Int_ty; 
    fun_name = mk_ident "f"; 
    args = arg_dec; 
    body = mk_stmt (Slist []);
  } in
  let ast = mk_stmt (Sfunc fdec) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let typecheck = fun () -> let _ = Typechecker.checkStmt ftab vtab ast in ()
  in assert_raises (mk_error "Function is missing a return statement") typecheck

 let test_typechecker_func_return_in_if_stmt test_ctxt =
  let arg_dec = []in 
  let fdec = {
    fun_type = Int_ty; 
    fun_name = mk_ident "f"; 
    args = arg_dec; 
    body = mk_stmt (Slist [mk_stmt (Sif ((mk_expr (EBool true)), (mk_stmt (Sreturn (mk_expr (EConst 1)))), (mk_stmt (Slist [] ))))]);
  } in
  let ast = mk_stmt (Sfunc fdec) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let typecheck = fun () -> let _ = Typechecker.checkStmt ftab vtab ast in () in 
  assert_raises (mk_error "Function is missing a return statement") typecheck

 let test_typechecker_func_return_incorrect_type test_ctxt =
  let arg_dec = []in 
  let fdec = {
    fun_type = Int_ty; 
    fun_name = mk_ident "f"; 
    args = arg_dec; 
    body = mk_stmt (Slist [mk_stmt (Sreturn (mk_expr (EBool true)))]);
  } in
  let ast = mk_stmt (Sfunc fdec) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let typecheck = fun () -> let _ = Typechecker.checkStmt ftab vtab ast in () in 
  assert_raises (mk_error "Return type does not match function declaration") typecheck

let test_typechecker_func_recursion test_ctxt =
  let arg_dec = []in 
  let fdec = {
    fun_type = Int_ty; 
    fun_name = mk_ident "f"; 
    args = arg_dec; 
    body = mk_stmt (Slist [mk_stmt (Sreturn (mk_expr (EFcall (mk_ident "f", []))))]);
  } in
  let ast = mk_stmt (Sfunc fdec) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let (_, _, typechecked_ast) = Typechecker.checkStmt ftab vtab ast in
  let expected_out = "INTf(){returnf()}" in
  let ttree_string = remove_whitespace (Pp_type.pp_stmt typechecked_ast) in
  assert_equal ~printer:(fun x -> x) expected_out ttree_string 

  
let fdec_tests = "fdecTests" >::: [
  "test_typechecker_func_two_args" >:: test_typechecker_func_two_args;
  "test_typechecker_func_duplicate_argname" >:: test_typechecker_func_duplicate_argname;
  "test_typechecker_func_no_args" >:: test_typechecker_func_no_args;
  "test_typechecker_func_no_args_no_return" >:: test_typechecker_func_no_args_no_return;
  "test_typechecker_func_return_only_in_if_stmt" >:: test_typechecker_func_return_in_if_stmt;
  "test_typechecker_func_return_incorrect_type" >:: test_typechecker_func_return_incorrect_type;
  "test_typechecker_func_recursion" >:: test_typechecker_func_recursion;
  
]