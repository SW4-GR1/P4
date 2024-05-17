open OUnit2
open Frontend
open Parser

(* For debugging :) *)
let token_to_string = function
  | ADD -> "ADD"
  | SUB -> "SUB"
  | MUL -> "MUL"
  | DIV -> "DIV"
  | MOD -> "MOD"
  | ASSIGN -> "ASSIGN"
  | ADD_ASSIGN -> "ADD_ASSIGN"
  | SUB_ASSIGN -> "SUB_ASSIGN"
  | MUL_ASSIGN -> "MUL_ASSIGN"
  | DIV_ASSIGN -> "DIV_ASSIGN"
  | LT -> "LT"
  | GT -> "GT"
  | EQ -> "EQ"
  | NEQ -> "NEQ"
  | LEQ -> "LEQ"
  | GEQ -> "GEQ"
  | INC -> "INC"
  | DEC -> "DEC"
  | AND -> "AND"
  | OR -> "OR"
  | NOT -> "NOT"
  | LPAREN -> "LPAREN"
  | LBRACKET -> "LBRACKET"
  | LBRACE -> "LBRACE"
  | COMMA -> "COMMA"
  | DOT -> "DOT"
  | END -> "END"
  | IF -> "IF"
  | ELSE -> "ELSE"
  | RETURN -> "RETURN"
  | EXPORT -> "EXPORT"
  | INT_TY -> "INT_TY"
  | FLOAT_TY -> "FLOAT_TY"
  | LONG_INT_TY -> "LONG_INT_TY"
  | LONG_FLOAT_TY -> "LONG_FLOAT_TY"
  | BOOL_TY -> "BOOL_TY"
  | LET -> "LET"
  | FOR -> "FOR"
  | WHILE -> "WHILE"
  | INT i -> "INT " ^ string_of_int i
  | FLOAT f -> "FLOAT " ^ string_of_float f
  | BOOL b -> "BOOL " ^ string_of_bool b
  | IDENT s -> "IDENT \"" ^ s ^ "\""
  | EOF -> "EOF"

let test_lexerT input expected_output _test_ctxt = 
  let lexbuf = Lexing.from_string input in
  let rec get_all_tokens lexbuf =
    match Lexer.token lexbuf with
  | EOF -> [EOF]
  | token -> token :: get_all_tokens lexbuf;
  in
  assert_equal ~printer:(fun tokens -> String.concat ", " (List.map token_to_string tokens))
               expected_output
               (get_all_tokens lexbuf)

let test_lexerE input expected_output _test_ctxt = 
  let lexbuf = Lexing.from_string input in
  let rec get_all_tokens lexbuf =
    match Lexer.token lexbuf with
  | EOF -> [EOF]
  | token -> token :: get_all_tokens lexbuf;
  in
  assert_raises expected_output (fun () -> get_all_tokens lexbuf)

let suite =
"LexerTestSuite">::: [
  "test arithmetic operators">:::
    [ "test addition operator">:: test_lexerT "+" [ADD; EOF];
      "test subtraction operator">:: test_lexerT "-" [SUB; EOF];
      "test multiplication operator">:: test_lexerT "*" [MUL; EOF];
      "test division operator">:: test_lexerT "/" [DIV; EOF];
      "test modulus operator">:: test_lexerT "%" [MOD; EOF];
    ];

  "test assignment operators">:::
    [ "test single equal operator">:: test_lexerT "=" [ASSIGN; EOF];
      "test compound assignment operators">:::
        [ "test addition assignment operator">:: test_lexerT "+=" [ADD_ASSIGN; EOF];
          "test subtraction assignment operator">:: test_lexerT "-=" [SUB_ASSIGN; EOF];
          "test multiplication assignment operator">:: test_lexerT "*=" [MUL_ASSIGN; EOF];
          "test division assignment operator">:: test_lexerT "/=" [DIV_ASSIGN; EOF];
        ];
    ];

  "test comparison operators">:::
    [ "test less than operator">:: test_lexerT "<" [LT; EOF];
      "test greater than operator">:: test_lexerT ">" [GT; EOF];
      "test equal operator">:: test_lexerT "==" [EQ; EOF];
      "test not equal operator">:: test_lexerT "!=" [NEQ; EOF];
      "test less than or equal operator">:: test_lexerT "<=" [LEQ; EOF];
      "test greater than or equal operator">:: test_lexerT ">=" [GEQ; EOF];
    ];

  "test miscellaneous operators">:::
    [ "test increment operator">:: test_lexerT "++" [INC; EOF];
      "test decrement operator">:: test_lexerT "--" [DEC; EOF];
      "test logical and operator">:: test_lexerT "and" [AND; EOF];
      "test logical or operator">:: test_lexerT "or" [OR; EOF];
      "test logical not operator">:: test_lexerT "not" [NOT; EOF];
    ];

  "test punctuation">:::
    [ "test parentheses">:: test_lexerT "(" [LPAREN; EOF];
      "test brackets">:: test_lexerT "[" [LBRACKET; EOF];
      "test braces">:: test_lexerT "{" [LBRACE; EOF];
      "test comma">:: test_lexerT "," [COMMA; EOF];
      "test dot">:: test_lexerT "." [DOT; EOF];
      "test semicolon">:: test_lexerT ";" [END; EOF];
    ];

  "test keywords and types">:::
    [ "test if keyword">:: test_lexerT "if" [IF; EOF];
      "test else keyword">:: test_lexerT "else" [ELSE; EOF];
      "test return keyword">:: test_lexerT "return" [RETURN; EOF];
      "test int type">:: test_lexerT "int" [INT_TY; EOF];
      "test float type">:: test_lexerT "float" [FLOAT_TY; EOF];
      "test long int type">:: test_lexerT "long_int" [LONG_INT_TY; EOF];
      "test long float type">:: test_lexerT "long_float" [LONG_FLOAT_TY; EOF];
      "test bool type">:: test_lexerT "bool" [BOOL_TY; EOF];
      "test let keyword">:: test_lexerT "let" [LET; EOF];
      "test for keyword">:: test_lexerT "for" [FOR; EOF];
      "test while keyword">:: test_lexerT "while" [WHILE; EOF];
      "test export keyword">:: test_lexerT "export" [EXPORT; EOF];
    ];

  "test literals and identifiers">:::
    [ "test integer literal">:: test_lexerT "123" [INT 123; EOF];
      "test float literal">:: test_lexerT "3.14" [FLOAT 3.14; EOF];
      "test boolean literals">:: test_lexerT "true false" [BOOL true; BOOL false; EOF];
      "test identifier">:: test_lexerT "my_var" [IDENT "my_var"; EOF];
      "test multiple tokens">:: test_lexerT "var1 var2" [IDENT "var1"; IDENT "var2"; EOF];
      "test identifiers with digits and underscores">:: test_lexerT "var1 var_2 _var" [IDENT "var1"; IDENT "var_2"; IDENT "_var"; EOF];
      "test various combinations">:: test_lexerT "var1 = 10; /* Assignment */ var2 *= 2.5" [IDENT "var1"; ASSIGN; INT 10; END; IDENT "var2"; MUL_ASSIGN; FLOAT 2.5; EOF];
    ];

  "test comments, empty inputs and whitespace">:::
    [ "test multi-line comment">:: test_lexerT "/* This is a multi-line \n comment */" [EOF];
      "test single-line comment">:: test_lexerT "// Single-line comment\n 123" [INT 123; EOF];
      "test empty input">:: test_lexerT "" [EOF];
      "test input with only whitespaces">:: test_lexerT "   \t\n" [EOF];
      "test input with only comments">:: test_lexerT "/* Comment */ // Another comment" [EOF];
    ];

  "test errors">:::
    [
    "test unterminated multi-line comment">:: test_lexerE "/* This is an unterminated comment\n 123" (Frontend.Lexer.Lexing_error "Lexer - unterminated multi-line comment");
    "test identifiers and keywords with special characters">:: test_lexerE "var! var? var$" (Frontend.Lexer.Lexing_error "Lexer - unexpected character");
    "test invalid tokens">:: test_lexerE "var1 = 10; /* Assignment */ var2 *= 2.5 $" (Frontend.Lexer.Lexing_error "Lexer - unexpected character");
    ]
]

(* let _ = run_test_tt_main suite  *)