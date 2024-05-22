open OUnit2
open Unit_testing

(*unittesting*)

let suite = "Unit tests" >::: [
  Lexer_tests.suite;
  Parser_tests.suite; 
  Typechecker_tests.suite;
  Backend_tests.suite;
]