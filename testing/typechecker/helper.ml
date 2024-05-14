open Str
open Frontend
open Frontend.Ast 

let dummy_loc_str = "line 0, start column -1, end column -1"
let mk_ident s = { id = s; id_loc = (Lexing.dummy_pos, Lexing.dummy_pos) }
let mk_expr e = { expr_node = e; expr_loc = (Lexing.dummy_pos, Lexing.dummy_pos) }

let mk_vtab = SymTab.fromList [] (* empty variable table *)
let mk_ftab = SymTab.fromList [] (* empty function table *)

let mk_error msg = (Typechecker.Error (dummy_loc_str, msg))
 
let remove_whitespace s = Str.global_replace (Str.regexp "[ \t\n\r]+") "" s