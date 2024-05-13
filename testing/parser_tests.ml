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
      let long_int a = 5;
      let int a = x + 1;
      a += 6;
      a = 1;"
      ("
      let long int a = 5
      let int a = (x + 1)
      ( a += 6 )
      ( a = 1 )
      ");
      
      "boolTest" >:: test_parser "
      let bool a = true;
      let bool a = hey;
      let bool b = 2;
      bool f() {
        return 1
      }

      let bool lasse = 3;"
      ("
      let bool a = true
      let bool a = hey
      let bool b = 2
      bool f() {
      ( return 1 )
      }
      
      let bool lasse = 3");
      
      "commentTest" >:: test_parser "
      4 + 5;
      //this is a single line comment

      /* this
      is
      a 
      multi
      line comment*/
      "
      (" (( 4 + 5 )) ");

      (* "conditionTest" >:: test_parser " *)
      (* 4 < 2; *)
      (* 4 > 2; *)
      (* 4 == 2; *)
      (* 4 != 2; *)
      (* 4 >= 2; *)
      (* 4 <= 2; *)
      (* " *)
      (* " *)
      (* ( (4 < 2) ) *)
      (* ( (4 > 2) ) *)
      (* ( (4 == 2) ) *)
      (* ( (4 != 2) ) *)
      (* ( (4 >= 2) ) *)
      (* ( (4 <= 2) )"; *)
      (*  *)
      (* "ExportTest" >:: test_parser " *)
      (* {  *)
      (*     export func;  *)
      (* } *)
      (* int func(int x) {}" *)
      (* " *)
      (* {export func; *)
      (* export x;} *)
      (* ( (1 + 2) )"; *)
      (*  *)
      (* "forTest" >:: test_parser "for(let *) int i = 0; i < 10; i = i + 1;) {
      (*   a;}" *)
      (* " *)
      (* for (let int i = 0; (i < 10); ( i  *)= (i + 1) )) {
      (*   ( a ) *)
      (* }"; *)
      (*  *)
      (* "functionTest" >:: test_parser " *)
      (* int f() { *)
      (*     return 1 *)
      (* } *)

      (* int f(int a, int b) { *)
      (*     a + b; *)
      (*     return 8 *)
      (* }" *)
      (* " *)
      (* int f() { *)
      (* ( return 1 ) *)
      (* } *)

      (* int f(int a, int b) { *)
      (* ( (a + b) ) *)
      (* ( return 8 ) *)
      (* } *)
      (* "; *)
    ]
