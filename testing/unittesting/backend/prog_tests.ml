open OUnit2
open Backend
open Frontend.Ttree
open Helper


let test_compile_module_empty _test_ctxt =
  let input = {exports = [];
  globals = Sglobal_list [];
  stmts = Sfundec_list []} 
  in
  let expected_wat = "(module)" in
  let generated_wat = Wat.to_string (Compile.compile input) in
  let stripped_wat = remove_whitespace generated_wat in
  print_endline stripped_wat;
  assert_equal expected_wat stripped_wat

let test_compile_module_one_gvardec _test_ctxt =
  let input = {exports = [];
  globals = Sglobal_list [(Sglobal_var (mk_gvdec Tint "x" (mk_expr (Econst 1) Tint)))];
  stmts = Sfundec_list []} in 
  let expected_wat = "(module(global$x(muti32)(i32.const1)))" in
  let generated_wat = Wat.to_string (Compile.compile input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat


let test_compile_module_two_gvardec _test_ctxt =
  let input = {exports = [];
  globals = Sglobal_list [(Sglobal_var (mk_gvdec Tint "x" (mk_expr (Econst 1) Tint)));
                          (Sglobal_var (mk_gvdec Tint "y" (mk_expr (Econst 2) Tint)))];
  stmts = Sfundec_list []} in
  let expected_wat = "(module(global$x(muti32)(i32.const1))(global$y(muti32)(i32.const2)))" in
  let generated_wat = Wat.to_string (Compile.compile input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat


let test_compile_module_one_fundec _test_ctxt =
  let input = {exports = [];
  globals = Sglobal_list [];
  stmts = Sfundec_list [(Sfunc (mk_fundec Tint "f" [] (Slist [Sreturn (mk_expr (Econst 1) Tint)])))]} in
  let expected_wat = "(module(func$f(resulti32)(return(i32.const1))))" in
  let generated_wat = Wat.to_string (Compile.compile input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat

let test_compile_module_two_fundec _test_ctxt =
  let input = {exports = [];
  globals = Sglobal_list [];
  stmts = Sfundec_list [(Sfunc (mk_fundec Tint "f" [] (Slist [Sreturn (mk_expr (Econst 1) Tint)])));
                         (Sfunc (mk_fundec Tint "g" [] (Slist [Sreturn (mk_expr (Econst 2) Tint)])))]} in
  let expected_wat = "(module(func$f(resulti32)(return(i32.const1)))(func$g(resulti32)(return(i32.const2))))" in
  let generated_wat = Wat.to_string (Compile.compile input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat

let test_compile_module_one_fundec_one_export _test_ctxt =
  let input = {exports = [Xexport "f"];
  globals = Sglobal_list [];
  stmts = Sfundec_list [(Sfunc (mk_fundec Tint "f" [] (Slist [Sreturn (mk_expr (Econst 1) Tint)])))]} in
  let expected_wat = "(module(export\"f\"(func$f))(func$f(resulti32)(return(i32.const1))))" in
  let generated_wat = Wat.to_string (Compile.compile input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat


let test_compile_module_one_gvar_fundec_export _test_ctxt =
  let input = {exports = [Xexport "f"];
  globals = Sglobal_list [(Sglobal_var (mk_gvdec Tint "x" (mk_expr (Econst 1) Tint)))];
  stmts = Sfundec_list [(Sfunc (mk_fundec Tint "f" [] (Slist [Sreturn (mk_expr (Econst 1) Tint)])))]} in
  let expected_wat = "(module(export\"f\"(func$f))(global$x(muti32)(i32.const1))(func$f(resulti32)(return(i32.const1))))" in
  let generated_wat = Wat.to_string (Compile.compile input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat


let test_compile_moduke_gvar_use_within_fun_body _test_ctxt =
  let input = {exports = [Xexport "f"];
  globals = Sglobal_list [(Sglobal_var (mk_gvdec Tint "x" (mk_expr (Econst 1) Tint)))];
  stmts = Sfundec_list [(Sfunc (mk_fundec Tint "f" [] (Slist [Sreturn (mk_expr (Eident "x") Tint)])))]} in
  let expected_wat = "(module(export\"f\"(func$f))(global$x(muti32)(i32.const1))(func$f(resulti32)(return(get_global$x))))" in
  let generated_wat = Wat.to_string (Compile.compile input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat



  let prog_tests = "BackendCompileTests" >::: [
    "test_compile_module_empty" >:: test_compile_module_empty;
    "test_compile_module_one_gvardec" >:: test_compile_module_one_gvardec;
    "test_compile_module_two_gvardec" >:: test_compile_module_two_gvardec;
    "test_compile_module_one_fundec" >:: test_compile_module_one_fundec;
    "test_compile_module_two_fundec" >:: test_compile_module_two_fundec;
    "test_compile_module_one_fundec_one_export" >:: test_compile_module_one_fundec_one_export;
    "test_compile_module_one_gvar_fundec_export" >:: test_compile_module_one_gvar_fundec_export;
    "test_compile_moduke_gvar_use_within_fun_body" >:: test_compile_moduke_gvar_use_within_fun_body;
    ]