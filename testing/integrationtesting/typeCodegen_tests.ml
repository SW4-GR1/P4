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


let suite =
  "TypeCodegen" >::: [
    "test_gvardec" >:: test_gvardec
  ]

  
