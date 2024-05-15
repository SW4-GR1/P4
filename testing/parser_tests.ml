open OUnit2
open Frontend
open Ast
open Parser
open Pp_parse
open Lexing
open Lexer

let remove_whitespace s = Str.global_replace (Str.regexp "[ \t\n\r]+") "" s
let dummy_lexbuf = Lexing.from_string ""

(* Mock lexer for testing of the parser *)
let mock_lexer tokens =
  let buffer = ref tokens in
  fun lexbuf ->
    match !buffer with
    | [] -> EOF
    | token::rest -> 
      buffer := rest;
      token
      
let test_parser input expected_output _ctxt =
  let parsed_input = Parser.prog (mock_lexer input) dummy_lexbuf in
  let result = remove_whitespace (Pp_parse.pp_prog parsed_input) in
  assert_equal ~printer:(fun x -> x) expected_output result

let test_parser_error input _ctxt =
  try
    let _ = Parser.prog (mock_lexer input) dummy_lexbuf in
    assert_failure "Expected parser to raise an error"
  with
    | Parser.Error -> ()
    | _ -> assert_failure "Expected parser to raise a Parser.Error exception"

let test_decl_without_global _ctxt =
  let input = [LET; INT_TY; IDENT "xy"; ASSIGN; INT 4; END] in
  test_parser_error input _ctxt

let test_global_decl_start_value _ctxt = 
  let input = [GLOBAL; INT_TY; IDENT "xy"; ASSIGN; INT 4; END] in
  let expected_output = "globalintxy=4" in
  test_parser input expected_output _ctxt

let test_global_decl_float_start_value _ctxt =
  let input = [GLOBAL; FLOAT_TY; IDENT "xy"; ASSIGN; FLOAT 4.0; END] in
  let expected_output = "globalfloatxy=4." in
  test_parser input expected_output _ctxt

let test_global_decl_long_int_start_value _ctxt =
  let input = [GLOBAL; LONG_INT_TY; IDENT "xy"; ASSIGN; INT 123456789; END] in
  let expected_output = "globallongintxy=123456789" in
  test_parser input expected_output _ctxt

let test_global_decl_long_float_start_value _ctxt =
  let input = [GLOBAL; LONG_FLOAT_TY; IDENT "xy"; ASSIGN; FLOAT 123456789.0; END] in
  let expected_output = "globallongfloatxy=123456789." in
  test_parser input expected_output _ctxt

  let test_global_decl_no_start_value _ctxt =
    let input = [GLOBAL; LET; INT_TY; IDENT "xy"; END] in
    test_parser_error input _ctxt

let test_global_addassign _ctxt =
  let input = [GLOBAL; INT_TY; IDENT "xy"; ADD_ASSIGN; INT 4; END] in
  test_parser_error input _ctxt

let test_global_binop _ctxt =
  let input = [INT 1; ADD; INT 2; END] in
  test_parser_error input _ctxt

let test_function_decl _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; RETURN; INT 1; END; RBRACE] in
  let expected_output = "intfoo(){(return1)}" in
  test_parser input expected_output _ctxt

let test_function_decl_with_args _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; INT_TY; IDENT "x"; RPAREN; LBRACE; RETURN; INT 1; END; RBRACE] in
  let expected_output = "intfoo(intx){(return1)}" in
  test_parser input expected_output _ctxt

let test_function_decl_with_args_and_body _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; INT_TY; IDENT "x"; RPAREN; LBRACE; LET; INT_TY; IDENT "y"; ASSIGN; INT 1; END; RETURN; IDENT "y"; END; RBRACE] in
  let expected_output = "intfoo(intx){letinty=1(returny)}" in
  test_parser input expected_output _ctxt

let test_function_all_binop _ctxt = 
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; INT_TY; IDENT "x"; ASSIGN; INT 1; ADD; INT 2; SUB; INT 1; MUL; INT 2; DIV; INT 1; MOD; INT 2; END; RETURN; IDENT "x"; END; RBRACE] in
  let expected_output = "intfoo(){letintx=((1+2)-(((1*2)/1)%2))(returnx)}" in
  test_parser input expected_output _ctxt

let test_function_all_assignment _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; INT_TY; IDENT "x"; ASSIGN; INT 1; END; IDENT "x"; ADD_ASSIGN; IDENT "x"; END; IDENT "x"; SUB_ASSIGN; IDENT "x"; END; IDENT "x"; MUL_ASSIGN; IDENT "x"; END; IDENT "x"; DIV_ASSIGN; IDENT "x"; END; RETURN; IDENT "x"; END; RBRACE] in
 let expected_output = "intfoo(){letintx=1(x+=x)(x-=x)(x*=x)(x/=x)(returnx)}" in
  test_parser input expected_output _ctxt

let test_function_loop_for _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; INT_TY; IDENT "x"; END; FOR; LPAREN; LET; INT_TY; IDENT "i"; ASSIGN; INT 0; END; IDENT "i"; LT; INT 10; END; IDENT "i"; ADD_ASSIGN; INT 1; RPAREN; LBRACE; IDENT "x"; ADD_ASSIGN; IDENT "i"; END; RBRACE; RBRACE] in
  let expected_output = "intfoo(){letintxfor(letinti=0;(i<10);(i+=1)){(x+=i)}}" in
  test_parser input expected_output _ctxt

let test_function_loop_while _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; INT_TY; IDENT "x"; END; WHILE; LPAREN; IDENT "x"; LT; INT 10; RPAREN; LBRACE; IDENT "x"; ADD_ASSIGN; INT 1; END; RBRACE; RBRACE] in
  let expected_output = "intfoo(){letintxwhile((x<10)){(x+=1)}}" in
  test_parser input expected_output _ctxt

let test_function_loop_while_error _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; INT_TY; IDENT "x"; END; WHILE; LPAREN; IDENT "x"; LT; INT 10; RPAREN; LBRACE; IDENT "x"; ADD_ASSIGN; INT 1; END; END] in
  test_parser_error input _ctxt

let test_function_loop_for_error _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; INT_TY; IDENT "x"; END; FOR; LPAREN; LET; INT_TY; IDENT "i"; ASSIGN; INT 0; END; IDENT "i"; LT; INT 10; END; IDENT "i"; ADD_ASSIGN; INT 1; RBRACE; RBRACE] in
  test_parser_error input _ctxt

let test_function_for_loop_in_while _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; INT_TY; IDENT "x"; END; WHILE; LPAREN; IDENT "x"; LT; INT 10; RPAREN; LBRACE; FOR; LPAREN; LET; INT_TY; IDENT "i"; ASSIGN; INT 0; END; IDENT "i"; LT; INT 10; END; IDENT "i"; INC; RPAREN; LBRACE; IDENT "x"; ADD_ASSIGN; IDENT "i"; END; RBRACE; RBRACE; RBRACE] in
  let expected_output = "intfoo(){letintxwhile((x<10)){for(letinti=0;(i<10);(i++)){(x+=i)}}}" in
  test_parser input expected_output _ctxt

let test_function_while_loop_in_for _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; INT_TY; IDENT "x"; END; FOR; LPAREN; LET; INT_TY; IDENT "i"; ASSIGN; INT 0; END; IDENT "i"; LT; INT 10; END; IDENT "i"; ADD_ASSIGN; INT 1; RPAREN; LBRACE; WHILE; LPAREN; IDENT "x"; LT; INT 10; RPAREN; LBRACE; IDENT "x"; ADD_ASSIGN; IDENT "i"; END; RBRACE; RBRACE; RBRACE] in
  let expected_output = "intfoo(){letintxfor(letinti=0;(i<10);(i+=1)){while((x<10)){(x+=i)}}}" in
  test_parser input expected_output _ctxt

let test_function_if _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; INT_TY; IDENT "x"; END; IF; LPAREN; IDENT "x"; RPAREN; LBRACE; IDENT "x"; ADD_ASSIGN; INT 1; END; RBRACE; RBRACE] in
  let expected_output = "intfoo(){letintxif(x){(x+=1)}}" in
  test_parser input expected_output _ctxt

let test_function_if_else _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; INT_TY; IDENT "x"; END; IF; LPAREN; IDENT "x"; RPAREN; LBRACE; IDENT "x"; ADD_ASSIGN; INT 1; END; RBRACE; ELSE; LBRACE; IDENT "x"; ADD_ASSIGN; INT 2; END; RBRACE; RBRACE] in
  let expected_output = "intfoo(){letintxif(x){(x+=1)}else{(x+=2)}}" in
  test_parser input expected_output _ctxt

let test_function_if_else_error _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; INT_TY; IDENT "x"; END; IF; LPAREN; IDENT "x"; RPAREN; LBRACE; IDENT "x"; ADD_ASSIGN; INT 1; END; ELSE; LBRACE; IDENT "x"; ADD_ASSIGN; INT 2; END; RBRACE; RBRACE] in
  test_parser_error input _ctxt

let test_function_if_error _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; INT_TY; IDENT "x"; END; IF; LPAREN; IDENT "x"; LBRACE; IDENT "x"; ADD_ASSIGN; INT 1; END; RBRACE; RBRACE] in
  test_parser_error input _ctxt

 let test_function_if_in_for _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; INT_TY; IDENT "x"; END; FOR; LPAREN; LET; INT_TY; IDENT "i"; ASSIGN; INT 0; END; IDENT "i"; LT; INT 10; END; IDENT "i"; ADD_ASSIGN; INT 1; RPAREN; LBRACE; IF; LPAREN; IDENT "x"; RPAREN; LBRACE; IDENT "x"; ADD_ASSIGN; INT 1; END; RBRACE; RBRACE;RBRACE] in
  let expected_output = "intfoo(){letintxfor(letinti=0;(i<10);(i+=1)){if(x){(x+=1)}}}" in
  test_parser input expected_output _ctxt

let test_function_if_in_while _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; INT_TY; IDENT "x"; END; WHILE; LPAREN; IDENT "x"; RPAREN; LBRACE; IF; LPAREN; IDENT "x"; RPAREN; LBRACE; IDENT "x"; ADD_ASSIGN; INT 1; END; RBRACE; RBRACE; RBRACE] in
  let expected_output = "intfoo(){letintxwhile(x){if(x){(x+=1)}}}" in
  test_parser input expected_output _ctxt

let test_global_bool_decl _ctxt =
  let input = [GLOBAL; BOOL_TY; IDENT "xy"; ASSIGN; BOOL true; END] in
  let expected_output = "globalboolxy=true" in
  test_parser input expected_output _ctxt

let test_function_increment _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; INT_TY; IDENT "x"; END; IDENT "x"; INC; END; RBRACE] in
  let expected_output = "intfoo(){letintx((x++))}" in
  test_parser input expected_output _ctxt

let test_function_decrement _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; INT_TY; IDENT "x"; END; IDENT "x"; DEC; END; RBRACE] in
  let expected_output = "intfoo(){letintx((x--))}" in
  test_parser input expected_output _ctxt

let test_function_for_loop_in_if _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; INT_TY; IDENT "x"; END; IF; LPAREN; IDENT "x"; RPAREN; LBRACE; FOR; LPAREN; LET; INT_TY; IDENT "i"; ASSIGN; INT 0; END; IDENT "i"; LT; INT 10; END; IDENT "i"; ADD_ASSIGN; INT 1; RPAREN; LBRACE; IDENT "x"; ADD_ASSIGN; IDENT "i"; END; RBRACE; RBRACE; RBRACE] in
  let expected_output = "intfoo(){letintxif(x){for(letinti=0;(i<10);(i+=1)){(x+=i)}}}" in
  test_parser input expected_output _ctxt

let test_function_while_loop_in_if _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; RPAREN; LBRACE; LET; INT_TY; IDENT "x"; END; IF; LPAREN; IDENT "x"; RPAREN; LBRACE; WHILE; LPAREN; IDENT "x"; LT; INT 10; RPAREN; LBRACE; IDENT "x"; ADD_ASSIGN; INT 1; END; RBRACE; RBRACE; RBRACE] in
  let expected_output = "intfoo(){letintxif(x){while((x<10)){(x+=1)}}}" in
  test_parser input expected_output _ctxt

let test_multiple_arguments_function _ctxt =
  let input = [INT_TY; IDENT "foo"; LPAREN; INT_TY; IDENT "x"; COMMA; INT_TY; IDENT "y"; RPAREN; LBRACE; RETURN; IDENT "x"; ADD; IDENT "y"; END; RBRACE] in
  let expected_output = "intfoo(intx,inty){(return(x+y))}" in
  test_parser input expected_output _ctxt

  (* Test Suite *)
let suite = "ParserTests" >::: [
  "test_parse_decl_without_global_fails" >:: test_decl_without_global;
  "test_parse_global_declaration_assigns_succueds" >:: test_global_decl_start_value;
  "test_parse_global_declaration_float_start_value" >:: test_global_decl_float_start_value;
  "test_parse_global_declaration_long_int_start_value" >:: test_global_decl_long_int_start_value;
  "test_parse_global_declaration_long_float_start_value" >:: test_global_decl_long_float_start_value;
  "test_parse_global_declaration_no_start_value" >:: test_global_decl_no_start_value;
  "test_parse_global_declaration_addassign_fails" >:: test_global_addassign;
  "test_parse_global_binop" >:: test_global_binop;
  "test_parse_function_decl" >:: test_function_decl;
  "test_parse_function_decl_with_args" >:: test_function_decl_with_args;
  "test_parse_function_decl_with_args_and_body" >:: test_function_decl_with_args_and_body;
  "test_parse_function_binop" >:: test_function_all_binop;
  "test_parse_function_all_assignment" >:: test_function_all_assignment;
  "test_parse_function_loop_for" >:: test_function_loop_for;
  "test_parse_function_loop_while" >:: test_function_loop_while;
  "test_parse_function_loop_while_error" >:: test_function_loop_while_error;
  "test_parse_function_loop_for_error" >:: test_function_loop_for_error;
  "test_parse_function_for_loop_in_while" >:: test_function_for_loop_in_while;
  "test_parse_function_while_loop_in_for" >:: test_function_while_loop_in_for;
  "test_parse_function_if" >:: test_function_if;
  "test_parse_function_if_else" >:: test_function_if_else;
  "test_parse_function_if_else_error" >:: test_function_if_else_error;
  "test_parse_function_if_error" >:: test_function_if_error;
  "test_parse_function_if_in_for" >:: test_function_if_in_for;
  "test_parse_function_if_in_while" >:: test_function_if_in_while;
  "test_parse_global_bool_decl" >:: test_global_bool_decl;
  "test_parse_function_increment" >:: test_function_increment;
  "test_parse_function_decrement" >:: test_function_decrement;
  "test_parse_function_for_loop_in_if" >:: test_function_for_loop_in_if;
  "test_parse_function_while_loop_in_if" >:: test_function_while_loop_in_if;
  "test_parse_multiple_arguments_function" >:: test_multiple_arguments_function;
  
]

