{
    open Parser
    open Lexing

exception Lexing_error of string

let next_line lexbuf = 
  let pos = lexbuf.lex_curr_p in 
  lexbuf.lex_curr_p <- 
    {
      pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum +1
    }

let kwd_table = [
  "if", IF;
  "else", ELSE;
  "return", RETURN;
  "int", INT_TY;
  "float", FLOAT_TY;
  "long_int", LONG_INT_TY;
  "long_float", LONG_FLOAT_TY;
  "bool", BOOL_TY;
  "let", LET;
  "for", FOR;
  "while", WHILE;
  ]

let id_or_kwd = 
    let h = Hashtbl.create 30 in
    List.iter (fun(s,t) -> Hashtbl.add h s t) kwd_table;
    fun s ->
        let s = String.lowercase_ascii s in 
        try List.assoc s kwd_table with _ -> IDENT s

}

let space = [' ' '\t']
let newline = ['\r'  '\n']
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let integer = digit+
let float = digit+ '.' digit+
let ident = (alpha) (alpha|digit)*
let bool = "true" | "false"

rule token = parse
  | newline             { next_line lexbuf; token lexbuf }
  | space+              { token lexbuf }
  | '='                 { ASSIGN }
  | "+="                { ADD_ASSIGN }
  | "-="                { SUB_ASSIGN }
  | "*="                { MUL_ASSIGN }
  | "/="                { DIV_ASSIGN }
  | '+'                 { ADD }
  | '-'                 { SUB }
  | '*'                 { MUL }
  | '/'                 { DIV }
  | '%'                 { MOD }
  | "++"                { INC }
  | "--"                { DEC }
  | "and"               { AND }
  | "or"                { OR }
  | "not"               { NOT }
  | '<'                 { LT }
  | '>'                 { GT }
  | "=="                { EQ }
  | "!="                { NEQ }
  | "<="                { LEQ }
  | ">="                { GEQ }
  | '('                 { LPAREN }
  | ')'                 { RPAREN }
  | '{'                 { LBRACE }
  | '}'                 { RBRACE }
  | '['                 { LBRACKET }
  | ']'                 { RBRACKET }
  | ','                 { COMMA }
  | '.'                 { DOT }
  | ';'                 { END }
  | "return"            { RETURN }
  | "export"            { EXPORT }
  | integer as c        { INT (int_of_string c) }
  | float as fl         { FLOAT (float_of_string fl) }
  | bool as bl          { BOOL (bool_of_string bl) }
  | ident as id         { id_or_kwd id }
  | "/*"                { multi_line_comment lexbuf }
  | "//"                { single_line_comment lexbuf }
  | eof                 { EOF }
  | _                   { raise (Lexing_error "Lexer - unexpected character") }

and multi_line_comment = parse 
  | "*/"                { token lexbuf }
  | newline             { next_line lexbuf; multi_line_comment lexbuf}
  | eof                 { raise(Lexing_error "Lexer - unterminated multi-line comment") }
  | _                   { multi_line_comment lexbuf }

and single_line_comment = parse 
  | newline             { next_line lexbuf; token lexbuf }
  | eof                 { EOF }
  | _                   { single_line_comment lexbuf }