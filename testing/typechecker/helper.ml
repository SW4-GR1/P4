open Str
open Frontend
open Frontend.Ast 

let dummy_loc_str = "line 0, start column -1, end column -1"
let mock_position = {
  Lexing.pos_fname = "mock_file";
  pos_lnum = 1;
  pos_bol = 0;
  pos_cnum = 1;
}

let dummy_loc = (mock_position, mock_position)

let mk_ident s = { id = s; id_loc = (Lexing.dummy_pos, Lexing.dummy_pos) }
let mk_expr e = { expr_node = e; expr_loc = (Lexing.dummy_pos, Lexing.dummy_pos) }

let mk_stmt st =   { stmt_node = st; stmt_loc =  (Lexing.dummy_pos, Lexing.dummy_pos) }

let mk_vtab = SymTab.fromList [] (* empty variable table *)
let mk_ftab = SymTab.fromList [] (* empty function table *)

let mk_error msg = (Typechecker.Error (dummy_loc_str, msg))
 
let remove_whitespace s = Str.global_replace (Str.regexp "[ \t\n\r]+") "" s