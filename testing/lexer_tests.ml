open OUnit2
open Lexing
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

let test_lexer input expected_output _test_ctxt = 
  let lexbuf = Lexing.from_string input in
  let rec get_all_tokens lexbuf =
    match Lexer.token lexbuf with
  | EOF -> [EOF]
  | token -> token :: get_all_tokens lexbuf;
  in
  assert_equal ~printer:(fun tokens -> String.concat ", " (List.map token_to_string tokens))
               expected_output
               (get_all_tokens lexbuf)
let test_unterminated_multiline_comment _ =
  let input = "/* This is an unterminated comment\n 123" in
  let lexbuf = Lexing.from_string input in
  let exn = Frontend.Lexer.Lexing_error "Lexer - unterminated multi-line comment" in
  assert_raises exn (fun () -> Lexer.token lexbuf)

let suite =
"LexerTestSuite">::: [
  "test arithmetic operators">:::
    [ "test addition operator">:: test_lexer "+" [ADD; EOF];
      "test subtraction operator">:: test_lexer "-" [SUB; EOF];
      "test multiplication operator">:: test_lexer "*" [MUL; EOF];
      "test division operator">:: test_lexer "/" [DIV; EOF];
      "test modulus operator">:: test_lexer "%" [MOD; EOF];
    ];

  "test assignment operators">:::
    [ "test single equal operator">:: test_lexer "=" [ASSIGN; EOF];
      "test compound assignment operators">:::
        [ "test addition assignment operator">:: test_lexer "+=" [ADD_ASSIGN; EOF];
          "test subtraction assignment operator">:: test_lexer "-=" [SUB_ASSIGN; EOF];
          "test multiplication assignment operator">:: test_lexer "*=" [MUL_ASSIGN; EOF];
          "test division assignment operator">:: test_lexer "/=" [DIV_ASSIGN; EOF];
        ];
    ];

  "test comparison operators">:::
    [ "test less than operator">:: test_lexer "<" [LT; EOF];
      "test greater than operator">:: test_lexer ">" [GT; EOF];
      "test equal operator">:: test_lexer "==" [EQ; EOF];
      "test not equal operator">:: test_lexer "!=" [NEQ; EOF];
      "test less than or equal operator">:: test_lexer "<=" [LEQ; EOF];
      "test greater than or equal operator">:: test_lexer ">=" [GEQ; EOF];
    ];

  "test miscellaneous operators">:::
    [ "test increment operator">:: test_lexer "++" [INC; EOF];
      "test decrement operator">:: test_lexer "--" [DEC; EOF];
      "test logical and operator">:: test_lexer "and" [AND; EOF];
      "test logical or operator">:: test_lexer "or" [OR; EOF];
      "test logical not operator">:: test_lexer "not" [NOT; EOF];
    ];

  "test punctuation">:::
    [ "test parentheses">:: test_lexer "(" [LPAREN; EOF];
      "test brackets">:: test_lexer "[" [LBRACKET; EOF];
      "test braces">:: test_lexer "{" [LBRACE; EOF];
      "test comma">:: test_lexer "," [COMMA; EOF];
      "test dot">:: test_lexer "." [DOT; EOF];
      "test semicolon">:: test_lexer ";" [END; EOF];
    ];

  "test keywords and types">:::
    [ "test if keyword">:: test_lexer "if" [IF; EOF];
      "test else keyword">:: test_lexer "else" [ELSE; EOF];
      "test return keyword">:: test_lexer "return" [RETURN; EOF];
      "test int type">:: test_lexer "int" [INT_TY; EOF];
      "test float type">:: test_lexer "float" [FLOAT_TY; EOF];
      "test long int type">:: test_lexer "long_int" [LONG_INT_TY; EOF];
      "test long float type">:: test_lexer "long_float" [LONG_FLOAT_TY; EOF];
      "test bool type">:: test_lexer "bool" [BOOL_TY; EOF];
      "test let keyword">:: test_lexer "let" [LET; EOF];
      "test for keyword">:: test_lexer "for" [FOR; EOF];
      "test while keyword">:: test_lexer "while" [WHILE; EOF];
    ];

  "test literals and identifiers">:::
    [ "test integer literal">:: test_lexer "123" [INT 123; EOF];
      "test float literal">:: test_lexer "3.14" [FLOAT 3.14; EOF];
      "test boolean literals">:: test_lexer "true false" [BOOL true; BOOL false; EOF];
      "test identifier">:: test_lexer "my_var" [IDENT "my_var"; EOF];
    ];

  "test comments">:::
    [ "test multi-line comment">:: test_lexer "/* This is a multi-line \n comment */" [EOF];
      "test single-line comment">:: test_lexer "// Single-line comment\n 123" [INT 123; EOF];
    ];

   "test unterminated multi-line comment">:: test_unterminated_multiline_comment;
  
]
