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

  | newline             { next_line lexbuf; token lexbuf }
  | space+              { token lexbuf }
  | '+'                 { ADD }
  | '-'                 { SUB }
  | '*'                 { MUL }
  | '/'                 { DIV }  
  | integer as lxm      { INT (int_of_string lxm) }
  | ident as id         { IDENT id }
  | "/*"                { multi_line_comment lexbuf }
  | "//"                { single_line_comment lexbuf }
  | eof                 { EOF }

and multi_line_comment = parse 
  | "*/"                { token lexbuf }
  | newline             { next_line lexbuf; multi_line_comment lexbuf}
  | eof                 { raise(Lexing_error "Lexer - unterminated multi-line comment") }
  | _                   { multi_line_comment lexbuf }

and single_line_comment = parse 
  | newline             { next_line lexbuf; token lexbuf }
  | eof                 { EOF }
  | _                   { single_line_comment lexbuf }



  (* let int[] x = [...,...];
     let int[4] x = [x1,x2,x3,x4]; 
     let matrix[3,3] mat;
     let matrix[3,3] mat = setMat([vec1, vec2, vec3]);
     let vec[3] ==> matrix[3,1]
     mat.setVec(1, newVec([7,6,6]));
     mat.setMat([vec1, vec2, vec3]);
     
    let vec[4] vecx = mat.getVec(2)
    
     let matrix[4,4] mat4;
     let vec[4]
      *)