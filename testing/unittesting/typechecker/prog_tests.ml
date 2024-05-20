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

let test_typecheck_valid_program ctxt =
  let ast = mk_prog ([
    Xexport "f"
  ]) (Sglobal_list []) (Sfundec_list [
    mk_stmt (Sfunc (mk_fundec Int_ty "f" [] (Slist [mk_stmt (Sreturn (mk_expr (EConst 1)))])))
  ]) in
  let expected_out = "INTf(){return(INT1)}" in 
  let typecheck = Typechecker.program ast in 
  let ttree_string = remove_whitespace (Pp_type.pp_prog typecheck) in
  assert_equal  ~printer:(fun x -> x) expected_out ttree_string




  let prog_tests = "Program typechecking tests" >::: [
    "Invalid export" >:: test_typecheck_invalid_export;
    "Test_typecheck_valid_program" >:: test_typecheck_valid_program
  ]  

  

