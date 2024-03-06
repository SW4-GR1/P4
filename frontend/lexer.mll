{
    open Token
    open Lexing

let next_line lexbuf = 
  let pos = lexbuf.lex_curr_p in 
  lexbuf.lex_curr_p <- 
    {
      pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum +1
    }


}

let space = [' ' '\t']
let newline = ['\r' '\n']
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let integer = '-'? digit+
let ident = (alpha) (alpha|digit)*

rule token = parse
  | space+              { token lexbuf }
  | newline+            { token lexbuf }
  | '+'                 { ADD }
  | '*'                 { MUL }
  | integer as lxm      { INT (int_of_string lxm) }
  | ident as id         { IDENT id }
  | "(*"                { comment lexbuf }
  | "//"                { line_comment lexbuf }
  | eof                 { EOF }

and comment = parse 
  | "*)"                { token lexbuf }
  | _                   { comment lexbuf }

and line_comment = parse 
  | '\n'                { token lexbuf }
  | _                   { line_comment lexbuf }