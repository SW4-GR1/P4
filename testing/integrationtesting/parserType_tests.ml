open OUnit2
open Frontend
open Pp_type
open Ast
open Parser
open Helper
open Lexing

(*dummy lexbuf*)
let dummy_lexbuf = Lexing.from_string ""

(*mock lexer*)
let mock_lexer tokens =
  let buffer = ref tokens in
  fun lexbuf ->
    match !buffer with
    | [] -> EOF
    | token::rest -> 
      buffer := rest;
      token


(*parser and typehecker tests*)
let test_parser_to_typechecker input expected_output _ctxt =
  let ast = Parser.prog (mock_lexer input) dummy_lexbuf in
  let expected_typed_output = remove_whitespace expected_output in
  let typed_output = remove_whitespace( Pp_type.pp_prog (Typechecker.program ast)) in
  assert_equal ~printer:(fun x -> x) expected_typed_output typed_output
  
(*parser to typechecker error*)
let test_parser_to_typechecker_error input expected_error _ctxt = 
  let actual_error = 
    try
      let _ = Typechecker.program (Parser.prog (mock_lexer input) dummy_lexbuf) in
      "Expected an error but recieved none"
    with
    | Typechecker.Error (_, msg) -> msg 
    | e -> Printexc.to_string e 
  in
  assert_equal ~printer:(fun x -> x) expected_error actual_error


(*unittests*)
let test_global_decl_int_start_value _ctxt = 
  let input = [GLOBAL; INT_TY; IDENT "x"; ASSIGN; INT 1; END] in
  let expected_output = "globalINTx=(INT1)" in
  test_parser_to_typechecker input expected_output _ctxt

let test_global_decl_float_start_value _ctxt =
  let input = [GLOBAL; FLOAT_TY; IDENT "x"; ASSIGN; FLOAT 4.0; END] in
  let expected_output = "globalFLOATx=(FLOAT4.)" in
  test_parser_to_typechecker input expected_output _ctxt

let test_global_decl_long_int_start_value _ctxt =
  let input = [GLOBAL; LONG_INT_TY; IDENT "x"; ASSIGN; INT 123456789; END] in
  let expected_output = "globalL_INTx=(INT123456789)" in
  test_parser_to_typechecker input expected_output _ctxt

let test_global_decl_long_float_start_value _ctxt =
  let input = [GLOBAL; LONG_FLOAT_TY; IDENT "x"; ASSIGN; FLOAT 123456789.0; END] in
  let expected_output = "globalL_FLOATx=(FLOAT123456789.)" in
  test_parser_to_typechecker input expected_output _ctxt
  
let test_global_decl_no_start_value _ctxt =
  let input = [GLOBAL; LET; INT_TY; IDENT "x"; END] in
  test_parser_to_typechecker input _ctxt

let test_global_decl_with_different_types _ctxt = 
  let input = [GLOBAL; INT_TY; IDENT "x"; ASSIGN; BOOL true; END;] in
  let expected_error = "Incompatible types int and bool" in
  test_parser_to_typechecker_error input expected_error _ctxt

let test_global_decl_with_same_name _ctxt = 
  let input = [GLOBAL; INT_TY; IDENT "x"; ASSIGN; INT 1; END;
               GLOBAL; INT_TY; IDENT "x"; ASSIGN; INT 2; END] in
  let expected_error = "Duplicate declaration of: x" in
  test_parser_to_typechecker_error input expected_error _ctxt

let test_global_compound_assignment _ctxt =
  let input = [GLOBAL; INT_TY; IDENT "x"; ASSIGN; INT 4; END;
               IDENT "x"; ADD_ASSIGN; INT 1; END] in
  let expected_output = "Frontend.Parser.MenhirBasics.Error" in
  test_parser_to_typechecker_error input expected_output _ctxt

let test_function_decl _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; RETURN; INT 1; END; RBRACE] in
  let expected_output = "INTfoo(){return(INT1)}" in
  test_parser_to_typechecker input expected_output _ctxt

let test_function_decl_with_args _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; INT_TY; IDENT "x"; RPAREN; LBRACE; RETURN; INT 1; END; RBRACE] in
  let expected_output = "INTfoo(INTx){return(INT1)}" in
  test_parser_to_typechecker input expected_output _ctxt

let test_function_decl_with_args_and_body _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; INT_TY; IDENT "x"; RPAREN; LBRACE; LET; INT_TY; IDENT "y"; ASSIGN; INT 1; END; RETURN; IDENT "y"; END; RBRACE] in
  let expected_output = "INTfoo(INTx){letINTy=(INT1)return(INTy)}" in
  test_parser_to_typechecker input expected_output _ctxt

let test_function_decl_no_return _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; INT_TY; IDENT "x"; ASSIGN; INT 1; END; RBRACE] in
  let expected_output = "Function is missing a return statement" in
  test_parser_to_typechecker_error input expected_output _ctxt

let test_function_decl_incompatible_return_types _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; RETURN; FLOAT 1.0; END; RBRACE] in
  let expected_output = "Return type does not match function declaration" in
  test_parser_to_typechecker_error input expected_output _ctxt

let test_function_decl_empty_body _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; RBRACE] in
  let expected_output = "Frontend.Parser.MenhirBasics.Error" in
  test_parser_to_typechecker_error input expected_output _ctxt

let test_function_all_binop _ctxt = 
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; INT_TY; IDENT "x"; ASSIGN; INT 1; ADD; INT 2; SUB; INT 1; MUL; INT 2; DIV; INT 1; MOD; INT 2; END; RETURN; IDENT "x"; END; RBRACE] in
  let expected_output = "INTfoo(){letINTx=(INT((INT((INT1)+(INT2)))-(INT((INT((INT((INT1)*(INT2)))/(INT1)))%(INT2)))))return(INTx)}" in
  test_parser_to_typechecker input expected_output _ctxt

let test_function_divion_by_zero_error _ctxt = 
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; INT_TY; IDENT "x"; ASSIGN; INT 1; DIV; INT 0; END; RETURN; IDENT "x"; END; RBRACE] in
  let expected_error = "Division by 0 not allowed" in
  test_parser_to_typechecker_error input expected_error _ctxt

let test_function_modulo_only_int _ctxt = 
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; INT_TY; IDENT "x"; ASSIGN; INT 1; MOD; FLOAT 2.0; END; RETURN; IDENT "x"; END; RBRACE] in
  let expected_error = "Modulo operator should only be used on integers" in
  test_parser_to_typechecker_error input expected_error _ctxt

let test_function_binary_only_numbers _ctxt = 
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; INT_TY; IDENT "x"; ASSIGN; INT 1; ADD; BOOL true; END; RETURN; IDENT "x"; END; RBRACE] in
  let expected_error = "Binary operator should only be used on numbers" in
  test_parser_to_typechecker_error input expected_error _ctxt

let test_function_negation _ctxt = 
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; INT_TY; IDENT "x"; ASSIGN; SUB; INT 1; END; RETURN; IDENT "x"; END; RBRACE] in
  let expected_output = "INTfoo(){letINTx=(INT((INT0)-(INT1)))return(INTx)}" in
  test_parser_to_typechecker input expected_output _ctxt

let test_function_negation_only_numbers _ctxt = 
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; INT_TY; IDENT "x"; ASSIGN; SUB; BOOL true; END; RETURN; IDENT "x"; END; RBRACE] in
  let expected_error = "Negation operator should only be used on numbers" in
  test_parser_to_typechecker_error input expected_error _ctxt

let test_function_all_compound_assignment _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; 
              LET; INT_TY; IDENT "x"; ASSIGN; INT 1; END;
              IDENT "x"; ADD_ASSIGN; IDENT "x"; END; 
              IDENT "x"; SUB_ASSIGN; IDENT "x"; END; 
              IDENT "x"; MUL_ASSIGN; IDENT "x"; END; 
              IDENT "x"; DIV_ASSIGN; IDENT "x"; END; 
              RETURN; IDENT "x"; END; RBRACE;] in
  let expected_output = "INTfoo(){letINTx=(INT1)x+=(INTx)x-=(INTx)x*=(INTx)x/=(INTx)return(INTx)}" in
  test_parser_to_typechecker input expected_output _ctxt

let test_function_increment_decrement _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; INT_TY; IDENT "x"; ASSIGN; INT 1; END; IDENT "x"; INC; END; IDENT "x"; DEC; END; RETURN; IDENT "x"; END; RBRACE;] in
  let expected_output = "INTfoo(){letINTx=(INT1)(INT(x++))(INT(x--))return(INTx)}" in
  test_parser_to_typechecker input expected_output _ctxt

let test_function_increment_decrement_error _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; BOOL_TY; IDENT "x"; END; IDENT "x"; INC; END; RETURN; IDENT "x"; DEC; END; RBRACE;] in
  let expected_error = "++ operator applied to a non-int type" in
  test_parser_to_typechecker_error input expected_error _ctxt

let test_function_loop_for _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; INT_TY; IDENT "x"; END; FOR; LPAREN; LET; INT_TY; IDENT "i"; ASSIGN; INT 0; END; IDENT "i"; LT; INT 10; END; IDENT "i"; ADD_ASSIGN; INT 1; RPAREN; LBRACE; IDENT "x"; ADD_ASSIGN; IDENT "i"; END; RBRACE; RETURN; INT 1; END; RBRACE;] in
  let expected_output = "INTfoo(){letINTxfor(letINTi=(INT0);(BOOL((INTi)<(INT10)));(i+=(INT1))){x+=(INTi)}return(INT1)}" in
  test_parser_to_typechecker input expected_output _ctxt

  let test_function_loop_while _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; INT_TY; IDENT "x"; END; WHILE; LPAREN; IDENT "x"; LT; INT 10; RPAREN; LBRACE; IDENT "x"; ADD_ASSIGN; INT 1; END; RBRACE; RETURN; INT 1; END; RBRACE] in
  let expected_output = "INTfoo(){letINTxwhile((BOOL((INTx)<(INT10)))){x+=(INT1)}return(INT1)}" in
  test_parser_to_typechecker input expected_output _ctxt

let test_function_loop_while_error_must_be_bool _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; INT_TY; IDENT "x"; END; WHILE; LPAREN; INT 1; RPAREN; LBRACE; IDENT "x"; ADD_ASSIGN; INT 1; END; RBRACE; RETURN; INT 1; END; RBRACE] in
  let expected_error = "Condition in while loop must be of type bool" in
  test_parser_to_typechecker_error input expected_error _ctxt

let test_function_loop_for_error_must_be_bool _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; INT_TY; IDENT "x"; END; FOR; LPAREN; LET; INT_TY; IDENT "i"; ASSIGN; INT 0; END; IDENT "x"; END; IDENT "i"; ADD_ASSIGN; INT 1; RPAREN; LBRACE; LET; INT_TY; IDENT "z"; ASSIGN; IDENT "i"; END; RBRACE; RETURN; INT 1; END; RBRACE;] in
  let expected_error = "Condition must evaluate to a boolean value" in
  test_parser_to_typechecker_error input expected_error _ctxt

let test_conditional_and _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; BOOL_TY; IDENT "x"; ASSIGN; LPAREN; BOOL true; AND; BOOL false; RPAREN; END; RETURN; INT 1; END; RBRACE] in
  let expected_output = "INTfoo(){letBOOLx=(BOOL((BOOLtrue)and(BOOLfalse)))return(INT1)}" in
test_parser_to_typechecker input expected_output _ctxt

let test_conditional_or _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; BOOL_TY; IDENT "x"; ASSIGN; LPAREN; BOOL true; OR; BOOL false; RPAREN; END; RETURN; INT 1; END; RBRACE] in
  let expected_output = "INTfoo(){letBOOLx=(BOOL((BOOLtrue)or(BOOLfalse)))return(INT1)}" in
test_parser_to_typechecker input expected_output _ctxt

  let test_conditional_not _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; BOOL_TY; IDENT "x"; ASSIGN; NOT; BOOL true; END; RETURN; INT 1; END; RBRACE] in
  let expected_output = "INTfoo(){letBOOLx=(BOOL(not(BOOLtrue)))return(INT1)}" in
test_parser_to_typechecker input expected_output _ctxt

let test_conditional_only_bool _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; BOOL_TY; IDENT "x"; ASSIGN; LPAREN; BOOL true; AND; INT 1; RPAREN; END; RETURN; INT 1; END; RBRACE] in
  let expected_error = "operator applied to a non-boolean type" in
  test_parser_to_typechecker_error input expected_error _ctxt

let test_conditional_not_only_bool _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; BOOL_TY; IDENT "x"; ASSIGN; NOT; INT 1; END; RETURN; INT 1; END; RBRACE] in
  let expected_error = "Not operator applied to a non-boolean type" in
  test_parser_to_typechecker_error input expected_error _ctxt

let test_export _ctxt =
  let input = [LBRACE; EXPORT; IDENT "foo"; END; RBRACE; INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; RETURN; INT 1; END; RBRACE;] in
  let expected_output = "{exportfoo;}INTfoo(){return(INT1)}" in
  test_parser_to_typechecker input expected_output _ctxt

let test_export_error _ctxt =
  let input = [LBRACE; EXPORT; IDENT "foo"; END; RBRACE;] in
  let expected_error = "Unbound function foo" in
  test_parser_to_typechecker_error input expected_error _ctxt


(*test suite*)
let suite = "ParserTypechecker_tests" >::: [
  "test_global_decl_start_value" >:: test_global_decl_int_start_value;
  "test_global_decl_float_start_value" >:: test_global_decl_float_start_value;
  "test_global_decl_long_int_start_value" >:: test_global_decl_long_int_start_value;
  "test_global_decl_long_float_start_value" >:: test_global_decl_long_float_start_value;
  "test_global_decl_with_different_types" >:: test_global_decl_with_different_types;
  "test_global_decl_with_same_name" >:: test_global_decl_with_same_name;
  "test_global_compound_assignment" >:: test_global_compound_assignment;
  "test_function_increment_decrement" >:: test_function_increment_decrement;
  "test_function_increment_decrement_error" >:: test_function_increment_decrement_error;
  "test_function_decl" >:: test_function_decl;
  "test_function_decl_with_args" >:: test_function_decl_with_args;
  "test_function_decl_with_args_and_body" >:: test_function_decl_with_args_and_body;
  "test_function_decl_no_return" >:: test_function_decl_no_return;
  "test_function_decl_incompatible_return_types" >:: test_function_decl_incompatible_return_types;
  "test_function_all_binop" >:: test_function_all_binop;
  "test_function_divion_by_zero_error" >:: test_function_divion_by_zero_error;
  "test_function_modulo_only_int" >:: test_function_modulo_only_int;
  "test_function_binary_only_numbers" >:: test_function_binary_only_numbers;
  "test_function_negation" >:: test_function_negation;
  "test_function_negation_only_numbers" >:: test_function_negation_only_numbers;
  "test_function_all_assignment" >:: test_function_all_compound_assignment;
  "test_function_loop_for" >:: test_function_loop_for;
  "test_function_loop_while" >:: test_function_loop_while;
  "test_function_loop_while_error" >:: test_function_loop_while_error_must_be_bool;
  "test_function_loop_for_error" >:: test_function_loop_for_error_must_be_bool;
  "test_conditional_and" >:: test_conditional_and;
  "test_conditional_or" >:: test_conditional_or;
  "test_conditional_not" >:: test_conditional_not;
  "test_conditional_only_bool" >:: test_conditional_only_bool;
  "test_conditional_not_only_bool" >:: test_conditional_not_only_bool;
  "test_export" >:: test_export;
  "test_export_error" >:: test_export_error;

]
