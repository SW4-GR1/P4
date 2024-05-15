open OUnit2

(*unittesting*)
let master_suite = "Master Suite" >::: [
  Lexer_tests.suite;
  Parser_tests.suite; 
  Typechecker_tests.suite;
  Backend_tests.suite;
]

let _ = run_test_tt_main master_suite