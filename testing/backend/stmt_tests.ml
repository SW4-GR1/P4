open OUnit2
open Backend
open Frontend.Ttree
open Helper

let test_compile_stmt_Ssimple_binop _test_ctxt =
  let stmt = Ssimple (mk_expr (Ebinop(Add, mk_expr(Econst 1) Tint, mk_expr(Econst 1) Tint)) Tint) in
  let expected =  "(drop(i32.add(i32.const1)(i32.const1)))" in
  let generated_wat = Wat.to_string (Compile.compile_stmt stmt) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected stripped_wat

let test_compile_stmt_Ssimple_unop _test_ctxt =
  let input = Ssimple (mk_expr (Eunop ("x", Inc)) Tint) in 
  let expected_wat = "(set_local$x(i32.add(get_local$x)(i32.const1)))(get_local$x)" in
  let generated_wat = Wat.to_string (Compile.compile_stmt input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat

let test_compile_stmt_Slist_empty _test_ctxt =
  let input = Slist [] in 
  let expected_wat = "" in
  let generated_wat = Wat.to_string (Compile.compile_stmt input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat

let test_compile_stmt_Slist_one _test_ctxt =
  let input = Slist [(Ssimple (mk_expr (Econst 1) Tint))] in 
  let expected_wat = "(drop(i32.const1))" in
  let generated_wat = Wat.to_string (Compile.compile_stmt input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat

let test_compile_stmt_Slist_two _test_ctxt =
  let input = Slist [(Ssimple (mk_expr (Econst 1) Tint)); (Ssimple (mk_expr (Econst 2) Tint))] in 
  let expected_wat = "(drop(i32.const1))(drop(i32.const2))" in
  let generated_wat = Wat.to_string (Compile.compile_stmt input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat
  

let stmt_tests = "BackendStmtTests" >::: [
    "test_compile_stmt_Ssimple" >:: test_compile_stmt_Ssimple_binop
  ; "test_compile_stmt_Ssimple_unop" >:: test_compile_stmt_Ssimple_unop
  ; "test_compile_stmt_Slist_empty" >:: test_compile_stmt_Slist_empty
  ; "test_compile_stmt_Slist_one" >:: test_compile_stmt_Slist_one
  ; "test_compile_stmt_Slist_two" >:: test_compile_stmt_Slist_two
] 