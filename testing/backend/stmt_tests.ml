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

let test_compile_stmt_function_declaration_no_args _test_ctxt =
  let input = Sfunc (mk_fundec Tint "main" [] (Slist [Ssimple (mk_expr (Econst 1) Tint)])) in
  let expected_wat = "(func$main(resulti32)(drop(i32.const1)))" in
  let generated_wat = Wat.to_string (Compile.compile_stmt input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat

let test_compile_stmt_function_declaration_one_arg _test_ctxt =
  let input = Sfunc (mk_fundec Tint "main" [(Tint, "x")] (Slist [Ssimple (mk_expr (Econst 1) Tint)])) in
  let expected_wat = "(func$main(param$xi32)(resulti32)(drop(i32.const1)))" in
  let generated_wat = Wat.to_string (Compile.compile_stmt input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat

let test_compile_stmt_function_declaration_two_args _test_ctxt =
  let input = Sfunc (mk_fundec Tint "main" [(Tint, "x"); (Tint, "y")] (Slist [Ssimple (mk_expr (Econst 1) Tint)])) in
  let expected_wat = "(func$main(param$xi32)(param$yi32)(resulti32)(drop(i32.const1)))" in
  let generated_wat = Wat.to_string (Compile.compile_stmt input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat

let test_compile_stmt_if _test_ctxt =
  let input = Sif (mk_expr (Ebool true) Tbool, Slist [Ssimple (mk_expr (Econst 1) Tint)], Slist []) in
  let expected_wat = "(if(i32.const1)(then(drop(i32.const1))))" in
  let generated_wat = Wat.to_string (Compile.compile_stmt input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat
   

let test_compile_stmt_if_else _test_ctxt =
  let input = Sif (mk_expr (Ebool true) Tbool, Slist [Ssimple (mk_expr (Econst 1) Tint)], Slist [Ssimple (mk_expr (Econst 2) Tint)]) in
  let expected_wat = "(if(i32.const1)(then(drop(i32.const1)))(else(drop(i32.const2))))" in
  let generated_wat = Wat.to_string (Compile.compile_stmt input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat


let test_compile_stmt_return _test_ctxt =
  let input = Sreturn (mk_expr (Econst 1) Tint) in
  let expected_wat = "(return(i32.const1))" in
  let generated_wat = Wat.to_string (Compile.compile_stmt input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat

let test_compile_stmt_vdec_start_value _test_ctxt =
  let input = Sdecl (mk_vdec Tint "x" (Some (mk_expr (Econst 1) Tint))) in
  let expected_wat = "(set_local$x(i32.const1))" in
  let generated_wat = Wat.to_string (Compile.compile_stmt input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat

let test_compile_stmt_vdec_no_start_value _test_ctxt =
  let input = Sdecl (mk_vdec Tint "x" None) in
  let expected_wat = "" in
  let generated_wat = Wat.to_string (Compile.compile_stmt input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat


let test_compile_stmt_assign_int_int _test_ctxt =
  let input = Sass ("x", Tint, Assign, (mk_expr (Econst 2) Tint)) in
  let expected_wat = "(set_local$x(i32.const2))" in
  let generated_wat = Wat.to_string (Compile.compile_stmt input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat

let test_compile_stmt_assign_longint_int _test_ctxt =
  let input = Sass ("x", Tlongint, Assign, (mk_expr (Econst 2) Tint)) in
  let expected_wat = "(set_local$x(i64.const2))" in
  let generated_wat = Wat.to_string (Compile.compile_stmt input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat
let test_compile_stmt_assign_longint_varofint _test_ctxt =
  let input = Sass ("x", Tlongint, Assign, (mk_expr (Eident "y") Tint)) in
  let expected_wat = "(set_local$x(i64.extend_i32_s(get_local$y)))" in
  let generated_wat = Wat.to_string (Compile.compile_stmt input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat

let test_compile_stmt_for_loop _test_ctxt =
  let input = Sfor 
  (Sdecl (mk_vdec Tint "x" (Some (mk_expr (Econst 1) Tint)))
  , mk_expr (Econd (Lt, (mk_expr (Eident "x") Tint) , (mk_expr (Econst 8) Tint))) Tint
  , Sass ("x", Tint, Add_assign, (mk_expr (Econst 1) Tint))
  , Slist [(Ssimple (mk_expr (Econst 1) Tint))] )  in
  let expected_wat = "(block(set_local$x(i32.const1))(loop(br_if1(i32.ge_s(get_local$x)(i32.const8)))(drop(i32.const1))(set_local$x(i32.add(get_local$x)(i32.const1)))(br0)))" in
  let generated_wat = Wat.to_string (Compile.compile_stmt input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat

let test_compile_stmt_while_loop _test_ctxt =
  let input = Swhile 
  (mk_expr (Econd (Lt, (mk_expr (Eident "x") Tint) , (mk_expr (Econst 8) Tint))) Tint
  , Slist [(Ssimple (mk_expr (Econst 1) Tint))] )  in
  let expected_wat = "(block(loop(br_if1(i32.ge_s(get_local$x)(i32.const8)))(drop(i32.const1))(br0)))" in
  let generated_wat = Wat.to_string (Compile.compile_stmt input) in
  let stripped_wat = remove_whitespace generated_wat in
  assert_equal expected_wat stripped_wat


let stmt_tests = "BackendStmtTests" >::: [
    "test_compile_stmt_Ssimple" >:: test_compile_stmt_Ssimple_binop
  ; "test_compile_stmt_Ssimple_unop" >:: test_compile_stmt_Ssimple_unop
  ; "test_compile_stmt_Slist_empty" >:: test_compile_stmt_Slist_empty
  ; "test_compile_stmt_Slist_one" >:: test_compile_stmt_Slist_one
  ; "test_compile_stmt_Slist_two" >:: test_compile_stmt_Slist_two
  ; "test_compile_stmt_function_declaration_no_args" >:: test_compile_stmt_function_declaration_no_args
  ; "test_compile_stmt_function_declaration_one_arg" >:: test_compile_stmt_function_declaration_one_arg
  ; "test_compile_stmt_function_declaration_two_args" >:: test_compile_stmt_function_declaration_two_args
  ; "test_compile_stmt_if" >:: test_compile_stmt_if
  ; "test_compile_stmt_if_else" >:: test_compile_stmt_if_else
  ; "test_compile_stmt_return" >:: test_compile_stmt_return
  ; "test_compile_stmt_vdec" >:: test_compile_stmt_vdec_start_value
  ; "test_compile_stmt_vdec_no_start_value" >:: test_compile_stmt_vdec_no_start_value
  ; "test_compile_stmt_assign_int_int" >:: test_compile_stmt_assign_int_int
  ; "test_compile_stmt_assign_longint_int" >:: test_compile_stmt_assign_longint_int
  ; "test_compile_stmt_assign_longint_varofint" >:: test_compile_stmt_assign_longint_varofint
  ; "test_compile_stmt_for_loop" >:: test_compile_stmt_for_loop
  ; "test_compile_stmt_while_loop" >:: test_compile_stmt_while_loop
] 