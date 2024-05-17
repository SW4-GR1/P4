open OUnit2
open Backend
open Frontend.Ttree
open Helper

let test_compile_const_int _test_ctxt =
  let input = mk_expr (Econst 1) Tint in 
  let expected_wat = "(i32.const1)" in
  let generated_wat = Wat.to_string (Compile.compile_expr input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat

let test_compile_longfloat _test_ctxt =
  let input = mk_expr (Efloat 1.1) Tlongfloat in 
  let expected_wat = "(f64.const1.100000)" in
  let generated_wat = Wat.to_string (Compile.compile_expr input) in
  let stripped_wat = remove_whitespace generated_wat in
  print_endline stripped_wat;
  assert_equal expected_wat stripped_wat

let test_compile_bool_true _test_ctxt =
  let input = mk_expr (Ebool true) Tbool in 
  let expected_wat = "(i32.const1)" in
  let generated_wat = Wat.to_string (Compile.compile_expr input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat

let test_compule_ident_local _test_ctxt =
  let input = mk_expr (Eident "x") Tint in 
  let expected_wat = "(get_local$x)" in
  let generated_wat = Wat.to_string (Compile.compile_expr input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat

let test_compile_binop_add _test_ctxt =
  let input = mk_expr (Ebinop (Add, mk_expr (Econst 1) Tint, mk_expr (Econst 1) Tint)) Tint in 
  let expected_wat = "(i32.add(i32.const1)(i32.const1))" in
  let generated_wat = Wat.to_string (Compile.compile_expr input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat

let test_compile_binop_div_int _test_ctxt =
  let input = mk_expr (Ebinop (Div, mk_expr (Econst 1) Tint, mk_expr (Econst 1) Tint)) Tint in 
  let expected_wat = "(i32.div_s(i32.const1)(i32.const1))" in
  let generated_wat = Wat.to_string (Compile.compile_expr input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat

let test_compile_binop_div_float _test_ctxt =
  let input = mk_expr (Ebinop (Div, mk_expr (Efloat 1.1) Tlongfloat, mk_expr (Efloat 1.1) Tlongfloat)) Tlongfloat in 
  let expected_wat = "(f64.div(f64.const1.100000)(f64.const1.100000))" in
  let generated_wat = Wat.to_string (Compile.compile_expr input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat

let test_compile_unop_inc _test_ctxt =
  let input = mk_expr (Eunop ("x", Inc)) Tint in 
  let expected_wat = "(set_local$x(i32.add(get_local$x)(i32.const1)))(get_local$x)" in
  let generated_wat = Wat.to_string (Compile.compile_expr input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat

let test_compile_unop_dec _test_ctxt =
  let input = mk_expr (Eunop ("x", Dec)) Tint in 
  let expected_wat = "(set_local$x(i32.sub(get_local$x)(i32.const1)))(get_local$x)" in
  let generated_wat = Wat.to_string (Compile.compile_expr input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat

let test_compile_log_and _test_ctxt =
  let input = mk_expr (Elog (And, mk_expr (Ebool true) Tbool, mk_expr (Ebool true) Tbool)) Tbool in 
  let expected_wat = "(i32.and(i32.const1)(i32.const1))" in
  let generated_wat = Wat.to_string (Compile.compile_expr input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat

let test_compile_log_or _test_ctxt =
  let input = mk_expr (Elog (Or, mk_expr (Ebool false) Tbool, mk_expr (Ebool true) Tbool)) Tbool in 
  let expected_wat = "(i32.or(i32.const0)(i32.const1))" in
  let generated_wat = Wat.to_string (Compile.compile_expr input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat

let test_compile_cond_eq_int _test_ctxt =
  let input = mk_expr (Econd (Eq, mk_expr (Econst 1) Tint, mk_expr (Econst 1) Tint)) Tint in 
  let expected_wat = "(i32.eq(i32.const1)(i32.const1))" in
  let generated_wat = Wat.to_string (Compile.compile_expr input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat

let test_compile_cond_eq_float _test_ctxt =
  let input = mk_expr (Econd (Eq, mk_expr (Efloat 1.1) Tlongfloat, mk_expr (Efloat 1.1) Tlongfloat)) Tlongfloat in 
  let expected_wat = "(f64.eq(f64.const1.100000)(f64.const1.100000))" in
  let generated_wat = Wat.to_string (Compile.compile_expr input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat

let test_compile_cond_gt_int _test_ctxt =
  let input = mk_expr (Econd (Gt, mk_expr (Econst 1) Tint, mk_expr (Econst 1) Tint)) Tint in 
  let expected_wat = "(i32.gt_s(i32.const1)(i32.const1))" in
  let generated_wat = Wat.to_string (Compile.compile_expr input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat

let test_compile_cond_gt_float _test_ctxt =
  let input = mk_expr (Econd (Gt, mk_expr (Efloat 1.1) Tlongfloat, mk_expr (Efloat 1.1) Tlongfloat)) Tlongfloat in 
  let expected_wat = "(f64.gt(f64.const1.100000)(f64.const1.100000))" in
  let generated_wat = Wat.to_string (Compile.compile_expr input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat

let test_compile_not_bool _test_ctxt =
  let input = mk_expr (Enot (mk_expr (Ebool true) Tbool)) Tbool in 
  let expected_wat = "(i32.eqz(i32.const1))" in
  let generated_wat = Wat.to_string (Compile.compile_expr input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat
let test_compile_not_ident_bool _test_ctxt =
  let input = mk_expr (Enot (mk_expr (Eident "x") Tbool)) Tbool in 
  let expected_wat = "(i32.eqz(get_local$x))" in
  let generated_wat = Wat.to_string (Compile.compile_expr input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat
let test_compile_fcall_0args _test_ctxt =
  let input = mk_expr (Efcall ("f", [])) Tint in 
  let expected_wat = "(call$f)" in
  let generated_wat = Wat.to_string (Compile.compile_expr input) in
  let stripped_wat = remove_whitespace generated_wat in
  print_endline stripped_wat;
  assert_equal expected_wat stripped_wat

let test_compile_fcall_1args _test_ctxt =
  let input = mk_expr (Efcall ("f", [mk_expr (Econst 1) Tint])) Tint in 
  let expected_wat = "(call$f(i32.const1))" in
  let generated_wat = Wat.to_string (Compile.compile_expr input) in
  let stripped_wat = remove_whitespace generated_wat in
  print_endline stripped_wat;
  assert_equal expected_wat stripped_wat

let test_compile_fcall_2args _test_ctxt =
  let input = mk_expr (Efcall ("f", [mk_expr (Econst 1) Tint; mk_expr (Econst 1) Tint])) Tint in 
  let expected_wat = "(call$f(i32.const1)(i32.const1))" in
  let generated_wat = Wat.to_string (Compile.compile_expr input) in
  let stripped_wat = remove_whitespace generated_wat in
  print_endline stripped_wat;
  assert_equal expected_wat stripped_wat


  let expr_tests = "BackendExprTests" >::: [
  "test_compile_const_int" >:: test_compile_const_int;
  "test_compile_longfloat" >:: test_compile_longfloat;
  "test_compile_bool_true" >:: test_compile_bool_true;
  "test_compule_ident_local" >:: test_compule_ident_local;
  "test_compile_binop_add" >:: test_compile_binop_add;
  "test_compile_binop_div_int" >:: test_compile_binop_div_int;
  "test_compile_binop_div_float" >:: test_compile_binop_div_float;
  "test_compile_unop_inc" >:: test_compile_unop_inc;
  "test_compile_unop_dec" >:: test_compile_unop_dec;
  "test_compile_log_and" >:: test_compile_log_and;
  "test_compile_log_or" >:: test_compile_log_or;
  "test_comile_cond_eq_int" >:: test_compile_cond_eq_int;
  "test_compile_cond_eq_float" >:: test_compile_cond_eq_float;
  "test_compile_cond_gt_int" >:: test_compile_cond_gt_int;
  "test_compile_cond_gt_float" >:: test_compile_cond_gt_float;
  "test_compile_not_bool" >:: test_compile_not_bool;
  "test_compile_not_ident_bool" >:: test_compile_not_ident_bool;
  "test_compile_fcall_0args" >:: test_compile_fcall_0args;
  "test_compile_fcall_1args" >:: test_compile_fcall_1args;
  "test_compile_fcall_2args" >:: test_compile_fcall_2args;
] 