open OUnit2
open Frontend
open Parser
open Lexing
open Ast
open Pp_parse

(* Function to print the parsetree in a readable format *)
let printer_function p =
  let parsetree = Pp_parse.pp_prog p in
  parsetree

(* Test the parser with a given input and expected output while removing whitespaces, newlines and so on *)
let test_parser input expected _test_ctxt =
  let lexbuf = Lexing.from_string input in
  let result = Parser.prog Lexer.token lexbuf in
  let result_str = printer_function result in
  let remove_whitespace s = Str.global_replace (Str.regexp "[ \t\n\r]+") "" s in
  assert_equal ~printer:(fun x -> x) (remove_whitespace expected) (remove_whitespace result_str)

let suite =
  "ParserTestSuite" >:::
    [
      "assignmentTest" >:: test_parser "
      global long_int a = 5;
      global int a = x + 1;
      int f() {
        let bool a = true;
        let bool b = false;
        let float a = 2.0;
        let long_float b = 3.0;
        }
      "
      ("
      int f(){
        let bool a = true
        let bool b = false
        let float a = 2. 
        let long float b = 3.
      }
      ");
    ]
