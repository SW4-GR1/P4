open OUnit2
open Integration_testing

let suite = "integrationTests" >:::[
  LexerParser_tests.suite;
  TypeCodegen_tests.suite;
  ParserType_tests.suite;
]

