open OUnit2
open Backend_testing


let suite = "BackendTests" >::: [
  Expr_tests.expr_tests;
  Stmt_tests.stmt_tests;
  Prog_tests.prog_tests;
] 


