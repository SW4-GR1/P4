open OUnit2
open Frontend
open Parser
open Lexing
open Pp_parse

(* Helper function to remove whitespaces from a string for easier comparison*)

let remove_whitespace s = Str.global_replace (Str.regexp "[ \t\n\r]+") "" s

(* Helper function to pretty print the AST from our lexer and parser*)
let pp_lexerParser str =
  let lexbuf = Lexing.from_string str in
  let ast = Parser.prog Lexer.token lexbuf in
  remove_whitespace (pp_prog ast)

  (*assert equal integration test of parser and lexer*)
let lexerparser_test input exspected _ctxt =
  assert_equal ~printer:(fun x -> x)  exspected (pp_lexerParser input)

  (* Test cases for lexer parser integrationtesting *)
let global_int_decl _ctxt=
  let input = "global int x = 1;" in
  let exspected = "globalintx=1" in
  lexerparser_test input exspected _ctxt

let global_float_decl _ctxt=
  let input = "global float x = 1.0;" in
  let exspected = "globalfloatx=1." in
  lexerparser_test input exspected _ctxt

let global_longInt_decl _ctxt=
  let input = "global long_int x = 1;" in
  let exspected = "globallongintx=1" in
  lexerparser_test input exspected _ctxt

let global_longFloat_decl _ctxt=
  let input = "global long_float x = 1.0;" in
  let exspected = "globallongfloatx=1." in
  lexerparser_test input exspected _ctxt

let global_bool_decl _ctxt=
  let input = "global bool x = true;" in
  let exspected = "globalboolx=true" in
  lexerparser_test input exspected _ctxt

let int_function_decl_test _ctxt=
  let input = "int x(){ return 1; }" in
  let exspected = "intx(){(return1)}" in
  lexerparser_test input exspected _ctxt

let float_function_decl_test _ctxt=
  let input = "float x(){ return 1.0; }" in
  let exspected = "floatx(){(return1.)}" in
  lexerparser_test input exspected _ctxt

let longInt_function_decl_test _ctxt=
  let input = "long_int x(){ return 1; }" in
  let exspected = "longintx(){(return1)}" in
  lexerparser_test input exspected _ctxt

let longFloat_function_decl_test _ctxt=
  let input = "long_float x(){ return 1.0; }" in
  let exspected = "longfloatx(){(return1.)}" in
  lexerparser_test input exspected _ctxt

let bool_function_decl_test _ctxt=
  let input = "bool x(){ return true; }" in
  let exspected = "boolx(){(returntrue)}" in
  lexerparser_test input exspected _ctxt

let test_function_decl_with_args _ctxt=
  let input = "int x(int a, float b){ return 1; }" in
  let exspected = "intx(inta,floatb){(return1)}" in
  lexerparser_test input exspected _ctxt

let test_function_decl_with_body_and_args _ctxt=
  let input = "int x(int a, float b){ let int c = 1; return c; }" in
  let exspected = "intx(inta,floatb){letintc=1(returnc)}" in
  lexerparser_test input exspected _ctxt

let test_decl_inside_fucntion _ctxt=
  let input = "int x(int a, float b){ let int c = 1; return c; }" in
  let exspected = "intx(inta,floatb){letintc=1(returnc)}" in
  lexerparser_test input exspected _ctxt

let test_function_for_loop _ctxt=
  let input = "int x(int a, float b){ for(let int i = 0; i < 10; i++){ return i; } }" in
  let exspected = "intx(inta,floatb){for(letinti=0;(i<10);(i++)){(returni)}}" in
  lexerparser_test input exspected _ctxt

let test_function_while_loop _ctxt=
  let input = "int x(int a, float b){ while(a < 10){ return a; } }" in
  let exspected = "intx(inta,floatb){while((a<10)){(returna)}}" in
  lexerparser_test input exspected _ctxt

let test_function_while_in_for _ctxt=
  let input = "int x(int a, float b){ for(let int i = 0; i < 10; i++){ while(a < 10){ return a; } } }" in
  let exspected = "intx(inta,floatb){for(letinti=0;(i<10);(i++)){while((a<10)){(returna)}}}" in
  lexerparser_test input exspected _ctxt

let test_function_for_in_while _ctxt=
  let input = "int x(int a, float b){ while(a < 10){ for(let int i = 0; i < 10; i++){ return a; } } }" in
  let exspected = "intx(inta,floatb){while((a<10)){for(letinti=0;(i<10);(i++)){(returna)}}}" in
  lexerparser_test input exspected _ctxt

let test_all_binop_function _ctxt=
  let input = "int x(int a, int b){ return a + b - a * b / a % b; }" in
  let exspected = "intx(inta,intb){(return((a+b)-(((a*b)/a)%b)))}" in
  lexerparser_test input exspected _ctxt

let test_function_if _ctxt=
  let input = "int x(int a, int b){ if(a < b){ return a; }}" in
  let exspected = "intx(inta,intb){if((a<b)){(returna)}}" in
  lexerparser_test input exspected _ctxt

let test_function_if_else _ctxt=
  let input = "int x(int a, int b){ if(a < b){ return a; } else { return b; }}" in
  let exspected = "intx(inta,intb){if((a<b)){(returna)}else{(returnb)}}" in
  lexerparser_test input exspected _ctxt

let test_function_if_in_for _ctxt=
  let input = "int x(int a, int b){ for(let int i = 0; i < 10; i++){ if(a < b){ return a; } } }" in
  let exspected = "intx(inta,intb){for(letinti=0;(i<10);(i++)){if((a<b)){(returna)}}}" in
  lexerparser_test input exspected _ctxt

let test_function_if_in_while _ctxt=
  let input = "int x(int a, int b){ while(a < b){ if(a < b){ return a; } } }" in
  let exspected = "intx(inta,intb){while((a<b)){if((a<b)){(returna)}}}" in
  lexerparser_test input exspected _ctxt

let test_function_if_else_in_for _ctxt=
  let input = "int x(int a, int b){ for(let int i = 0; i < 10; i++){ if(a < b){ return a; } else { return b; } } }" in
  let exspected = "intx(inta,intb){for(letinti=0;(i<10);(i++)){if((a<b)){(returna)}else{(returnb)}}}" in
  lexerparser_test input exspected _ctxt

let test_function_if_else_in_while _ctxt=
  let input = "int x(int a, int b){ while(a < b){ if(a < b){ return a; } else { return b; } } }" in
  let exspected = "intx(inta,intb){while((a<b)){if((a<b)){(returna)}else{(returnb)}}}" in
  lexerparser_test input exspected _ctxt

let test_function_add_assign _ctxt=
  let input = "int x(int a, int b){ a += b; return a; }" in
  let exspected = "intx(inta,intb){(a+=b)(returna)}" in
  lexerparser_test input exspected _ctxt

let test_function_sub_assign _ctxt=
  let input = "int x(int a, int b){ a -= b; return a; }" in
  let exspected = "intx(inta,intb){(a-=b)(returna)}" in
  lexerparser_test input exspected _ctxt

let test_function_mul_assign _ctxt=
  let input = "int x(int a, int b){ a *= b; return a; }" in
  let exspected = "intx(inta,intb){(a*=b)(returna)}" in
  lexerparser_test input exspected _ctxt

let test_function_div_assign _ctxt=
  let input = "int x(int a, int b){ a /= b; return a; }" in
  let exspected = "intx(inta,intb){(a/=b)(returna)}" in
  lexerparser_test input exspected _ctxt

let test_increment _ctxt=
  let input = "int x(int a, int b){ a++; return a; }" in
  let exspected = "intx(inta,intb){((a++))(returna)}" in
  lexerparser_test input exspected _ctxt

let test_decrement _ctxt=
  let input = "int x(int a, int b){ a--; return a; }" in
  let exspected = "intx(inta,intb){((a--))(returna)}" in
  lexerparser_test input exspected _ctxt

let test_increment_in_loop _ctxt=
  let input = "int x(int a, int b){ for(let int i = 0; i < 10; i++){ a++; } return a; }" in
  let exspected = "intx(inta,intb){for(letinti=0;(i<10);(i++)){((a++))}(returna)}" in
  lexerparser_test input exspected _ctxt

let test_decrement_in_loop _ctxt=
  let input = "int x(int a, int b){ for(let int i = 0; i < 10; i++){ a--; } return a; }" in
  let exspected = "intx(inta,intb){for(letinti=0;(i<10);(i++)){((a--))}(returna)}" in
  lexerparser_test input exspected _ctxt

(* Test cases for parser error testing *)

(* Test suite for lexer parser integration testing *)
let suite = "LexerParser" >::: [
    "Assert Equal Integration test" >:::[
    "Global Int Decl test" >:: global_int_decl;
    "Global Float Decl test" >:: global_float_decl;
    "Global Long Int Decl test" >:: global_longInt_decl;
    "Global Long Float Decl test" >:: global_longFloat_decl;
    "Global Bool Decl test" >:: global_bool_decl;
    "Int function Decl test" >:: int_function_decl_test;
    "Float function Decl test" >:: float_function_decl_test;
    "Long Int function Decl test" >:: longInt_function_decl_test;
    "Long Float function Decl test" >:: longFloat_function_decl_test;
    "Bool function Decl test" >:: bool_function_decl_test;
    "Test function Decl with args" >:: test_function_decl_with_args;
    "Test function Decl with body and args" >:: test_function_decl_with_body_and_args;
    "Test decl inside function" >:: test_decl_inside_fucntion;
    "Test function for loop" >:: test_function_for_loop;
    "Test function while loop" >:: test_function_while_loop;
    "Test function while in for" >:: test_function_while_in_for;
    "Test function for in while" >:: test_function_for_in_while;
    "Test all binop function" >:: test_all_binop_function;
    "Test function if" >:: test_function_if;
    "Test function if else" >:: test_function_if_else;
    "Test function if in for" >:: test_function_if_in_for;
    "Test function if in while" >:: test_function_if_in_while;
    "Test function if else in for" >:: test_function_if_else_in_for;
    "Test function if else in while" >:: test_function_if_else_in_while;
    "Test function add assign" >:: test_function_add_assign;
    "Test function sub assign" >:: test_function_sub_assign;
    "Test function mul assign" >:: test_function_mul_assign;
    "Test function div assign" >:: test_function_div_assign;
    "Test increment" >:: test_increment;
    "Test decrement" >:: test_decrement;
    "Test increment in loop" >:: test_increment_in_loop;
    "Test decrement in loop" >:: test_decrement_in_loop;
    
    ];
]