open OUnit2 
open Test_utils.Mkast
open Test_utils.Strutils 
open Frontend
open Frontend.Ast
open Backend


let typeCodegen_compile input_ast =
  let t_prog = Typechecker.program input_ast in
  let wat_prog = Compile.compile t_prog in
  remove_whitespace (Wat.to_string wat_prog)

let typeCodegen_test input_ast expected_output =
  assert_equal ~printer:(fun x -> x) expected_output (typeCodegen_compile input_ast)


let test_gvardec _ctxt =
  let global_var = mk_stmt (Sglobal_var (mk_gvdec Int_ty (mk_ident "x") (mk_expr (EConst 1)))) in
  let input_ast = (mk_prog [] (Sglobal_list([global_var])) (Sfundec_list([]))) in
  let expected_wat = "(module(global$x(muti32)(i32.const1)))" in
  typeCodegen_test input_ast expected_wat

let test_function _ctxt =
  let input_ast = mk_prog [] (Sglobal_list([])) (Sfundec_list [
    mk_stmt (Sfunc (mk_fundec Int_ty "f" [] (Slist [mk_stmt (Sreturn (mk_expr (EConst 1)))])))
  ]) in
  let expected_wat = "(module(func$f(resulti32)(return(i32.const1))))" in
  typeCodegen_test input_ast expected_wat

let test_function_and_export _ctxt =
  let exports = [(Xexport "f")] in 
  let globals = Sglobal_list [] in
  let functions = Sfundec_list [
    mk_stmt (Sfunc (mk_fundec Int_ty "f" [] (Slist [mk_stmt (Sreturn (mk_expr (EConst 1)))])))
  ] in
  let input_ast = mk_prog exports globals functions in
  let expected_wat = "(module(export\"f\"(func$f))(func$f(resulti32)(return(i32.const1))))" in
  typeCodegen_test input_ast expected_wat

let test_export_global_and_function _ctxt =
  let exports = [(Xexport "f")] in 
  let globals = Sglobal_list [
    mk_stmt (Sglobal_var (mk_gvdec Int_ty (mk_ident "x") (mk_expr (EConst 1))))
  ] in
  let functions = Sfundec_list [
    mk_stmt (Sfunc (mk_fundec Int_ty "f" [] (Slist [mk_stmt (Sreturn (mk_expr (EConst 1)))])))
  ] in
  let input_ast = mk_prog exports globals functions in
  let expected_wat = "(module(export\"f\"(func$f))(global$x(muti32)(i32.const1))(func$f(resulti32)(return(i32.const1))))" in
  typeCodegen_test input_ast expected_wat

let test_multiple_globals _ctxt =
  let globals = Sglobal_list [
    mk_stmt (Sglobal_var (mk_gvdec Int_ty (mk_ident "x") (mk_expr (EConst 1))));
    mk_stmt (Sglobal_var (mk_gvdec Int_ty (mk_ident "y") (mk_expr (EConst 2))))
  ] in
  let input_ast = mk_prog [] globals (Sfundec_list []) in
  let expected_wat = "(module(global$x(muti32)(i32.const1))(global$y(muti32)(i32.const2)))" in
  typeCodegen_test input_ast expected_wat

let test_multiple_functions _ctxt =
  let functions = Sfundec_list [
    mk_stmt (Sfunc (mk_fundec Int_ty "f" [] (Slist [mk_stmt (Sreturn (mk_expr (EConst 1)))])));
    mk_stmt (Sfunc (mk_fundec Int_ty "g" [] (Slist [mk_stmt (Sreturn (mk_expr (EConst 2)))])))
  ] in
  let input_ast = mk_prog [] (Sglobal_list []) functions in
  let expected_wat = "(module(func$f(resulti32)(return(i32.const1)))(func$g(resulti32)(return(i32.const2))))" in
  typeCodegen_test input_ast expected_wat

let test_multiple_functions_and_exports _ctxt =
  let exports = [(Xexport "f"); (Xexport "g")] in
  let functions = Sfundec_list [
    mk_stmt (Sfunc (mk_fundec Int_ty "f" [] (Slist [mk_stmt (Sreturn (mk_expr (EConst 1)))])));
    mk_stmt (Sfunc (mk_fundec Int_ty "g" [] (Slist [mk_stmt (Sreturn (mk_expr (EConst 2)))])))
  ] in
  let input_ast = mk_prog exports (Sglobal_list []) functions in
  let expected_wat = "(module(export\"f\"(func$f))(export\"g\"(func$g))(func$f(resulti32)(return(i32.const1)))(func$g(resulti32)(return(i32.const2))))" in
  typeCodegen_test input_ast expected_wat


let test_multiple_exports_globals_and_functions _ctxt =
  let exports = [(Xexport "f"); (Xexport "g")] in
  let globals = Sglobal_list [
    mk_stmt (Sglobal_var (mk_gvdec Int_ty (mk_ident "x") (mk_expr (EConst 1))));
    mk_stmt (Sglobal_var (mk_gvdec Int_ty (mk_ident "y") (mk_expr (EConst 2))))
  ] in
  let functions = Sfundec_list [
    mk_stmt (Sfunc (mk_fundec Int_ty "f" [] (Slist [mk_stmt (Sreturn (mk_expr (EConst 1)))])));
    mk_stmt (Sfunc (mk_fundec Int_ty "g" [] (Slist [mk_stmt (Sreturn (mk_expr (EConst 2)))])))
  ] in
  let input_ast = mk_prog exports globals functions in
  let expected_wat = "(module(export\"f\"(func$f))(export\"g\"(func$g))(global$x(muti32)(i32.const1))(global$y(muti32)(i32.const2))(func$f(resulti32)(return(i32.const1)))(func$g(resulti32)(return(i32.const2))))" in
  typeCodegen_test input_ast expected_wat

let test_bunch_of_binop_in_func_body _ctxt =
  let binop1 = mk_stmt (Ssimple (mk_expr (EBinop(Add, mk_expr (EConst 1), mk_expr (EConst 2)))))  in
  let binop2 = mk_stmt (Ssimple (mk_expr (EBinop(Sub, mk_expr (EConst 3), mk_expr (EConst 4)))))  in
  let binop3 = mk_stmt (Ssimple (mk_expr (EBinop(Mul, mk_expr (EFloat 5.0), mk_expr (EFloat 6.4)))))  in
  let binop4 = mk_stmt (Ssimple (mk_expr (EBinop(Div, mk_expr (EConst 8), mk_expr (EConst 4)))))  in
  let binop5 = mk_stmt (Ssimple (mk_expr (EBinop(Mod, mk_expr (EConst 9), mk_expr (EConst 4)))))  in
  let return = mk_stmt (Sreturn (mk_expr (EConst 1))) in
  let functions = [mk_stmt (Sfunc (mk_fundec Int_ty "f" [] (Slist [binop1; binop2; binop3; binop4; binop5; return])))] in
  let input_ast = mk_prog [] (Sglobal_list []) (Sfundec_list functions) in
  let expected_wat = "(module(func$f(resulti32)(drop(i32.add(i32.const1)(i32.const2)))(drop(i32.sub(i32.const3)(i32.const4)))(drop(f32.mul(f32.const5.000000)(f32.const6.400000)))(drop(i32.div_s(i32.const8)(i32.const4)))(drop(i32.rem_s(i32.const9)(i32.const4)))(return(i32.const1))))" in
  typeCodegen_test input_ast expected_wat

let test_bunch_of_cond_expr_in_func_body _ctxt =
  let cond1 = mk_stmt (Ssimple (mk_expr (ECond (Lt, mk_expr (EConst 2), mk_expr (EConst 3)))))  in
  let cond2 = mk_stmt (Ssimple (mk_expr (ECond (Gt, mk_expr (EFloat 4.2), mk_expr (EFloat 4.1)))))  in
  let cond3 = mk_stmt (Ssimple (mk_expr (ECond (Eq, mk_expr (EBool true), mk_expr (EBool false)))))  in
  let cond4 = mk_stmt (Ssimple (mk_expr (ECond (Neq, mk_expr (EConst 8), mk_expr (EConst 8)))))  in
  let cond5 = mk_stmt (Ssimple (mk_expr (ECond (Leq, mk_expr (EConst 9), mk_expr (EConst 10)))))  in
  let cond6 = mk_stmt (Ssimple (mk_expr (ECond (Geq, mk_expr (EConst 11), mk_expr (EConst 12)))))  in
  let return = mk_stmt (Sreturn (mk_expr (EConst 1))) in
  let functions = [mk_stmt (Sfunc (mk_fundec Int_ty "f" [] (Slist [cond1; cond2; cond3; cond4; cond5; cond6; return])))] in
  let input_ast = mk_prog [] (Sglobal_list []) (Sfundec_list functions) in
  let expected_wat = "(module(func$f(resulti32)(drop(i32.lt_s(i32.const2)(i32.const3)))(drop(f32.gt(f32.const4.200000)(f32.const4.100000)))(drop(i32.eq(i32.const1)(i32.const0)))(drop(i32.ne(i32.const8)(i32.const8)))(drop(i32.le_s(i32.const9)(i32.const10)))(drop(i32.ge_s(i32.const11)(i32.const12)))(return(i32.const1))))" in
  typeCodegen_test input_ast expected_wat



  (* Need magnus prime on this *)
let test_bunch_of_vdecs_and_assignments_in_func_body _ctxt =
  let vdec1 = mk_stmt (Sdecl {var_ty = Int_ty; var_name = mk_ident "y"; var_expr = None}) in
  let vdec2 = mk_stmt (Sdecl {var_ty = Int_ty; var_name = mk_ident "z"; var_expr = None}) in
  let vdec3 = mk_stmt (Sdecl {var_ty = Int_ty; var_name = mk_ident "a"; var_expr = None}) in
  let assign1 = mk_stmt (Sass ("y", Add_assign, mk_expr(EConst 1))) in
  let assign2 = mk_stmt (Sass ("z", Sub_assign, mk_expr(EConst 2))) in
  let assign3 = mk_stmt (Sass ("a", Mul_assign, mk_expr(EConst 3))) in
  let return = mk_stmt (Sreturn (mk_expr (EIdent (mk_ident "y")))) in
  let functions = [mk_stmt (Sfunc (mk_fundec Int_ty "f" [] (Slist [vdec1; vdec2; vdec3; assign1; assign2; assign3; return])))] in
  let input_ast = mk_prog [] (Sglobal_list []) (Sfundec_list functions) in
  let expected_wat = "(module(func$f(resulti32)(local$ai32)(local$zi32)(local$yi32)(set_local$y(i32.add(get_local$y)(i32.const1)))(set_local$z(i32.sub(get_local$z)(i32.const2)))(set_local$a(i32.mul(get_local$a)(i32.const3)))(return(get_local$y))))" in
  typeCodegen_test input_ast expected_wat


let test_vars_with_similar_names _ctxt =
  let globals = Sglobal_list [
    mk_stmt (Sglobal_var (mk_gvdec Int_ty (mk_ident "xyz") (mk_expr (EConst 1))));
  ] in 
  let vdec1 = mk_stmt (Sdecl {var_ty = Int_ty; var_name = mk_ident "x"; var_expr = None}) in
  let vdec2 = mk_stmt (Sdecl {var_ty = Int_ty; var_name = mk_ident "y"; var_expr = None}) in
  let vdec3 = mk_stmt (Sdecl {var_ty = Int_ty; var_name = mk_ident "z"; var_expr = None}) in
  let vdec4 = mk_stmt (Sdecl {var_ty = Int_ty; var_name = mk_ident "xy"; var_expr = None}) in
  let vdec5 = mk_stmt (Sdecl {var_ty = Int_ty; var_name = mk_ident "yz"; var_expr = None}) in
  let vdec6 = mk_stmt (Sdecl {var_ty = Int_ty; var_name = mk_ident "xz"; var_expr = None}) in
  let return = mk_stmt (Sreturn (mk_expr (EConst 1))) in
  let functions = [mk_stmt (Sfunc (mk_fundec Int_ty "f" [] (Slist [vdec1; vdec2; vdec3; vdec4; vdec5; vdec6; return])))] in
  let input_ast = mk_prog [] globals (Sfundec_list functions) in
  let expected_wat = "(module(global$xyz(muti32)(i32.const1))(func$f(resulti32)(local$zi32)(local$yi32)(local$xzi32)(local$yzi32)(local$xi32)(local$xyi32)(return(i32.const1))))" in
  typeCodegen_test input_ast expected_wat
  
(* 3 2 6 5 1 4
   order locals are declared   *)


(* Test Suite *)  

let suite =
  "TypeCodegen" >::: [
    "test_gvardec" >:: test_gvardec
  ; "test_function" >:: test_function
  ; "test_function_and_export" >:: test_function_and_export
  ; "test_export_global_and_function" >:: test_export_global_and_function
  ; "test_multiple_globals" >:: test_multiple_globals
  ; "test_multiple_functions" >:: test_multiple_functions
  ; "test_multiple_functions_and_exports" >:: test_multiple_functions_and_exports
  ; "test_multiple_exports_globals_and_functions" >:: test_multiple_exports_globals_and_functions
  ; "test_bunch_of_binop_in_func_body" >:: test_bunch_of_binop_in_func_body
  ; "test_bunch_of_cond_expr_in_func_body" >:: test_bunch_of_cond_expr_in_func_body
  ; "test_bunch_of_vdecs_and_assignments_in_func_body" >:: test_bunch_of_vdecs_and_assignments_in_func_body
  ; "test_vars_with_similar_names" >:: test_vars_with_similar_names
  
  ]

  
