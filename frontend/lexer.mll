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
    [' ' '\t' '\n']      { token lexbuf }
    |  ['+']             { ADD }
    |  ['*']             { MUL }
    |  ['0'-'9']+ as lxm { INT (int_of_string lxm) }
    |  ['_']             {token lexbuf }
    |  ["(*"]            {comment lexbuf}
    |  ["//" ]           {line_comment lexbuf}
    | eof                { EOF }

    and comment = parse 
    ["*)"]               {token lexbuf}
    |  ['_']             {comment lexbuf}

    and line_comment = parse 
    ["\n"]               {token lexbuf}
    | ['_']              {line_comment lexbuf}