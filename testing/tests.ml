open OUnit2

(*unittesting*)
let master_suite = "Master Suite" >::: [
  Unit_tests.suite;
  Integration_tests.suite;
]

let _ = run_test_tt_main master_suite