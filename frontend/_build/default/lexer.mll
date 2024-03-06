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

let space = ' ' | '\t'
let comment = "//" [^'\n']* | "(*"
let newline = '\r' | '\n' | "\r\n"

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let integer = '-'? digit+
let ident = (alpha) (alpha|digit|'_')*


rule token = parse
[' ' '\t' '\n']
    { token lexbuf }
|  ['+']
    { ADD }
|  ['*']
    { MUL }
|  digit+ as lxm
    { INT (int_of_string lxm) }
| eof
    { EOF }