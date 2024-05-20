open OUnit2
open Frontend 
open Frontend.Ast
open Test_utils.Mkast
open Test_utils.Strutils
open Test_utils.Typecheckerutils


let test_typecheck_invalid_export ctxt =
  let ast = mk_prog ([
    Xexport "f"
  ]) (Sglobal_list []) (Sfundec_list []) in
  let typecheck = fun () -> let _ = Typechecker.program ast in ()
  in assert_raises (mk_error ~loc:"Unknown location" "Unbound function f") typecheck

let test_typecheck_valid_export ctxt =
  let ast = mk_prog ([
    Xexport "f"
  ]) (Sglobal_list []) (Sfundec_list [
    mk_stmt (Sfunc (mk_fundec Int_ty "f" [] (Slist [mk_stmt (Sreturn (mk_expr (EConst 1)))])))
  ]) in
  let expected_out = "INTf(){return(INT1)}" in 
  let typecheck = Typechecker.program ast in 
  let ttree_string = remove_whitespace (Pp_type.pp_prog typecheck) in
  assert_equal  ~printer:(fun x -> x) expected_out ttree_string

let test_typecheck_globarvar_funarg_duplicate_name ctxt =
  let ast = mk_prog ([
  ]) (Sglobal_list [
    mk_stmt (Sglobal_var (mk_gvdec Int_ty (mk_ident "x") (mk_expr (EConst 1))));]) 
    (Sfundec_list [ mk_stmt (Sfunc (mk_fundec Int_ty "f" [(Int_ty, (mk_ident "x"))] (Slist [mk_stmt (Sreturn (mk_expr (EConst 1)))])))]) in
  let typecheck = fun () -> let _ = Typechecker.program ast in () in
  assert_raises (mk_error "Function argument x has been declared elsewhere") typecheck


let test_typecheck_duplicate_func_name ctxt =
  let ast = mk_prog ([
  ]) (Sglobal_list []) 
    (Sfundec_list [ mk_stmt (Sfunc (mk_fundec Int_ty "f" [] (Slist [mk_stmt (Sreturn (mk_expr (EConst 1)))])));
                     mk_stmt (Sfunc (mk_fundec Int_ty "f" [] (Slist [mk_stmt (Sreturn (mk_expr (EConst 1)))])))]) in
  let typecheck = fun () -> let _ = Typechecker.program ast in () in
  assert_raises (mk_error "Duplicate declaration of: f") typecheck


let test_typecheck_gvar_func_same_name ctxt =
  let ast = mk_prog ([
  ]) (Sglobal_list [
    mk_stmt (Sglobal_var (mk_gvdec Int_ty (mk_ident "x") (mk_expr (EConst 1))));]) 
    (Sfundec_list [ mk_stmt (Sfunc (mk_fundec Int_ty "x" [] (Slist [mk_stmt (Sreturn (mk_expr (EConst 1)))])))]) in
  let typecheck = fun () -> let _ = Typechecker.program ast in () in
  assert_raises (mk_error "Duplicate declaration of: x") typecheck

let test_typecheck_use_of_gvar_in_funcbody ctxt =
  let ast = mk_prog ([
  ]) (Sglobal_list [
    mk_stmt (Sglobal_var (mk_gvdec Int_ty (mk_ident "x") (mk_expr (EConst 1))));]) 
    (Sfundec_list [ mk_stmt (Sfunc (mk_fundec Int_ty "f" [] (Slist [mk_stmt (Sreturn (mk_expr (EIdent(mk_ident "x"))))])))]) in
  let expected_out = "globalINTx=(INT1)INTf(){return(INTx)}" in
  let typecheck = Typechecker.program ast in
  let ttree_string = remove_whitespace (Pp_type.pp_prog typecheck) in
  assert_equal  ~printer:(fun x -> x) expected_out ttree_string

let test_typecheck_decl_of_var_in_fun_body_with_same_name_as_global_var ctxt =
  let ast = mk_prog ([
  ]) (Sglobal_list [
    mk_stmt (Sglobal_var (mk_gvdec Int_ty (mk_ident "x") (mk_expr (EConst 1))));]) 
    (Sfundec_list [ (mk_stmt (Sfunc (mk_fundec Int_ty "f" [] (Slist [(mk_stmt (Sdecl { var_ty = Int_ty; var_name = mk_ident "x"; var_expr = None })); (mk_stmt (Sreturn (mk_expr (EConst 1)))) ]))))]) in
  let typecheck = fun () -> let _ = Typechecker.program ast in () in
  assert_raises (mk_error "Duplicate declaration of: x") typecheck


  let prog_tests = "Program typechecking tests" >::: [
    "Invalid export" >:: test_typecheck_invalid_export;
    "Valid export" >:: test_typecheck_valid_export;
    "Global var and function argument with same name" >:: test_typecheck_globarvar_funarg_duplicate_name;
    "Duplicate function name" >:: test_typecheck_duplicate_func_name;
    "Global var and function with same name" >:: test_typecheck_gvar_func_same_name;
    "Use of global var in function body" >:: test_typecheck_use_of_gvar_in_funcbody;
    "Decl of var in fun body with same name as global var" >:: test_typecheck_decl_of_var_in_fun_body_with_same_name_as_global_var;
  ]  

  

