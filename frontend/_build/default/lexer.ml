# 1 "lexer.mll"
 
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

let kwd_table = 
["if", IF;]

let id_or_kwd = 
    let h = Hashtbl.create 30 in
    List.iter (fun(s,t) -> Hashtbl.add h s t) kwd_table;
    fun s ->
        let s = String.lowercase_ascii s in 
        try List.assoc s kwd_table with _ -> IDENT s


# 28 "lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\245\255\075\000\150\000\016\000\251\255\160\000\253\255\
    \002\000\255\255\246\255\247\255\002\000\252\255\253\255\254\255\
    \012\000\255\255\004\000\253\255\254\255\255\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\007\000\006\000\005\000\255\255\003\000\255\255\
    \001\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \003\000\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_default =
   "\255\255\000\000\255\255\255\255\255\255\000\000\255\255\000\000\
    \255\255\000\000\000\000\000\000\013\000\000\000\000\000\000\000\
    \255\255\000\000\019\000\000\000\000\000\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\008\000\009\000\008\000\015\000\009\000\021\000\015\000\
    \000\000\021\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \008\000\000\000\008\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\005\000\007\000\016\000\006\000\000\000\004\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\011\000\017\000\000\000\000\000\000\000\010\000\
    \000\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\000\000\000\000\000\000\000\000\002\000\
    \000\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\000\000\000\000\
    \000\000\000\000\002\000\000\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \001\000\000\000\014\000\000\000\020\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\008\000\012\000\000\000\018\000\012\000\
    \255\255\018\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\008\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\000\000\012\000\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\004\000\016\000\255\255\255\255\255\255\004\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\255\255\255\255\255\255\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\255\255\255\255\
    \255\255\255\255\002\000\255\255\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\012\000\255\255\018\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec token lexbuf =
   __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 35 "lexer.mll"
                        ( next_line lexbuf; token lexbuf )
# 171 "lexer.ml"

  | 1 ->
# 36 "lexer.mll"
                        ( token lexbuf )
# 176 "lexer.ml"

  | 2 ->
# 37 "lexer.mll"
                        ( ADD )
# 181 "lexer.ml"

  | 3 ->
# 38 "lexer.mll"
                        ( SUB )
# 186 "lexer.ml"

  | 4 ->
# 39 "lexer.mll"
                        ( MUL )
# 191 "lexer.ml"

  | 5 ->
# 40 "lexer.mll"
                        ( DIV )
# 196 "lexer.ml"

  | 6 ->
let
# 41 "lexer.mll"
               c
# 202 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 41 "lexer.mll"
                        ( INT (int_of_string c) )
# 206 "lexer.ml"

  | 7 ->
let
# 42 "lexer.mll"
             id
# 212 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 42 "lexer.mll"
                        ( id_or_kwd id )
# 216 "lexer.ml"

  | 8 ->
# 43 "lexer.mll"
                        ( multi_line_comment lexbuf )
# 221 "lexer.ml"

  | 9 ->
# 44 "lexer.mll"
                        ( single_line_comment lexbuf )
# 226 "lexer.ml"

  | 10 ->
# 45 "lexer.mll"
                        ( EOF )
# 231 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and multi_line_comment lexbuf =
   __ocaml_lex_multi_line_comment_rec lexbuf 12
and __ocaml_lex_multi_line_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 48 "lexer.mll"
                        ( token lexbuf )
# 243 "lexer.ml"

  | 1 ->
# 49 "lexer.mll"
                        ( next_line lexbuf; multi_line_comment lexbuf)
# 248 "lexer.ml"

  | 2 ->
# 50 "lexer.mll"
                        ( raise(Lexing_error "Lexer - unterminated multi-line comment") )
# 253 "lexer.ml"

  | 3 ->
# 51 "lexer.mll"
                        ( multi_line_comment lexbuf )
# 258 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_multi_line_comment_rec lexbuf __ocaml_lex_state

and single_line_comment lexbuf =
   __ocaml_lex_single_line_comment_rec lexbuf 18
and __ocaml_lex_single_line_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 54 "lexer.mll"
                        ( next_line lexbuf; token lexbuf )
# 270 "lexer.ml"

  | 1 ->
# 55 "lexer.mll"
                        ( EOF )
# 275 "lexer.ml"

  | 2 ->
# 56 "lexer.mll"
                        ( single_line_comment lexbuf )
# 280 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_single_line_comment_rec lexbuf __ocaml_lex_state

;;

