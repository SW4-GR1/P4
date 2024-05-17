open OUnit2
open Helper 
open Frontend 
open Frontend.Ast
open Frontend.Ttree

let test_typechecker_func_no_args_no_body test_ctxt =
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
  print_endline ("Expected: " ^ expected_out);
  print_endline ("Actual: " ^ ttree_string);
  assert_equal expected_out ttree_string

let fdec_tests = "fdecTests" >::: [
  "test_typechecker_func_no_args_no_body" >:: test_typechecker_func_no_args_no_body;

]