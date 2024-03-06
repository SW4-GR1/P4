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
}

let space = ' '| '\t'
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let integer = '-'? digit+
let ident = (alpha) (alpha|digit)*

rule token = parse

(* [' ' '\t' '\n']
    { token lexbuf }
|  ['+']
    { ADD }
|  ['*']
    { MUL }
|  digit+ as lxm
    { INT (int_of_string lxm) }
| eof
    { EOF } *)

  | newline             {next_line lexbuf; token lexbuf }
  | space+              { token lexbuf }
  | '+'                 { ADD }
  | '*'                 { MUL }
  | integer as lxm      { INT (int_of_string lxm) }
  | ident as id         { IDENT id }
  | "(*"                { comment lexbuf }
  | "//"                { line_comment lexbuf }
  | eof                 { EOF }

and comment = parse 
  | "*)"                { token lexbuf }
  | newline             { next_line lexbuf; comment lexbuf}
  | eof                 { raise(Lexing_error "Lexer - unterminated multi-line comment") }
  | _                   { comment lexbuf }

and line_comment = parse 
  | newline             { next_line lexbuf; token lexbuf }
  | eof                 { EOF }
  | _                   { line_comment lexbuf }