open Frontend
open Frontend.Ast
open OUnit2


(* hjælpe funktioner til mocking af Ast *)

let dummy_loc_str = "line 0, start column -1, end column -1"
let mk_ident s = { id = s; id_loc = (Lexing.dummy_pos, Lexing.dummy_pos) }
let mk_expr e = { expr_node = e; expr_loc = (Lexing.dummy_pos, Lexing.dummy_pos) }

let mk_vtab = SymTab.fromList [] (* empty variable table *)
let mk_ftab = SymTab.fromList [] (* empty function table *)

let mk_error msg = (Typechecker.Error (dummy_loc_str, msg))
 
let remove_whitespace s = Str.global_replace (Str.regexp "[ \t\n\r]+") "" s

let test_typecheck_binop_add_const _test_ctxt = 
  (* Create an AST representing the expression "1 + 1" *)
  let ast = mk_expr (EBinop (Add, mk_expr (EConst 1), mk_expr (EConst 1))) in
  let expected_out = "(INT((INT1)+(INT1)))" in
  (* Typecheck the AST *)
  let ftab = mk_ftab in 
  let vtab = mk_vtab in
  let (ty, expr) = Typechecker.checkExp ftab vtab ast in
  let ttree_string = remove_whitespace (Pp_type.pp_expr expr) in
  print_endline ttree_string;
  assert_equal Ttree.Tint ty;
  assert_equal expected_out ttree_string

let test_typecheck_binop_add_const_and_float _test_ctxt =
  (* Create an AST representing the expression "1 + 1.0" *)
  let ast = mk_expr (EBinop (Add, mk_expr (EConst 1), mk_expr (EFloat 1.0))) in
  (* Typecheck the AST *)
  let ftab = mk_ftab in 
  let vtab = mk_vtab in
  let typecheck = fun () -> let _ = Typechecker.checkExp ftab vtab ast in ()
  in
  assert_raises (mk_error "Incompatible types int and float") typecheck
  
let test_typecheck_binop_add_const_and_bool _test_ctxt =
  (* Create an AST representing the expression "1 + true" *)
  let ast = mk_expr (EBinop (Add, mk_expr (EConst 1), mk_expr (EBool true))) in
  (* Typecheck the AST *)
  let ftab = mk_ftab in 
  let vtab = mk_vtab in
  let typecheck = fun () -> let _ = Typechecker.checkExp ftab vtab ast in ()
  in
  assert_raises (mk_error "Binary operator should only be used on numbers") typecheck

let test_typecheck_binop_mod_float _test_ctxt =
  let ast = mk_expr (EBinop (Mod, mk_expr (EConst 1), mk_expr (EFloat 1.0))) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let typecheck = fun () -> let _ = Typechecker.checkExp ftab vtab ast in ()
  in assert_raises (mk_error "Modulo operator should only be used on integers") typecheck

let test_typecheck_binop_div_0 _test_ctxt =
  let ast = mk_expr (EBinop (Div, mk_expr (EConst 1), mk_expr (EConst 0))) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let typecheck = fun () -> let _ = Typechecker.checkExp ftab vtab ast in ()
  in assert_raises (mk_error "Division by 0 not allowed") typecheck


(* In a perfect world the compiler raises an error if divisor evals to 0*)
let test_typechecker_binop_div_eval0 _test_ctxt =
  let ast = mk_expr (EBinop (Div, mk_expr (EConst 1), mk_expr (EBinop (Sub, mk_expr (EConst 1), mk_expr (EConst 1))))) in
  let expected_out = "(INT((INT1)/(INT((INT1)-(INT1)))))" in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let (_, expr) = Typechecker.checkExp ftab vtab ast in 
  let ttree_string = remove_whitespace (Pp_type.pp_expr expr) in
  assert_equal expected_out ttree_string

let test_typechecker_datatype_const _test_ctxt =
  let ast = mk_expr (EConst 1) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let (ty, expr) = Typechecker.checkExp ftab vtab ast in
  assert_equal Ttree.Tint ty;
  assert_equal "(INT1)" (remove_whitespace (Pp_type.pp_expr expr))

let test_typechecker_datatype_float _test_ctxt =
  let ast = mk_expr (EFloat 1.1) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let (ty, expr) = Typechecker.checkExp ftab vtab ast in
  assert_equal Ttree.Tfloat ty;
  assert_equal "(FLOAT1.1)" (remove_whitespace (Pp_type.pp_expr expr))

let test_typechecker_datatype_bool _test_ctxt =
  let ast = mk_expr (EBool true) in
  let ftab = mk_ftab in
  let vtab = mk_vtab in
  let (ty, expr) = Typechecker.checkExp ftab vtab ast in
  assert_equal Ttree.Tbool ty;
  assert_equal "(BOOLtrue)" (remove_whitespace (Pp_type.pp_expr expr))


let datatype_tests = "DatatypeTests" >::: [
  "test_typecheck:bool" >:: test_typechecker_datatype_bool;
  "test_typecheck:int" >:: test_typechecker_datatype_const;
  "test_typecheck:float" >:: test_typechecker_datatype_float;
]

let binop_tests = "BinopTests" >::: [
  "test_typecheck:add 1+1" >:: test_typecheck_binop_add_const;
  "test_typecheck:add 1+1.0_incompatible_types" >:: test_typecheck_binop_add_const_and_float;
  "test_typecheck:add 1+true :raises error" >:: test_typecheck_binop_add_const_and_bool;
  "test_typecheck:mod 1%1.0_raises error" >:: test_typecheck_binop_mod_float;
  "test_typecheck:div 1/0_raises error" >:: test_typecheck_binop_div_0;
  "test_typecheck:div 1/(1-1)_raises error" >:: test_typechecker_binop_div_eval0;
  (* Add more tests here *)
] 

let suite = "TypecheckerTests" >::: [
  datatype_tests;
  binop_tests;
    (* Add more tests here *)
] 


  
(* let _ = run_test_tt_main suite 
 *)