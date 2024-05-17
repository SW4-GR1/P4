open Frontend.Ast



let mock_position = {
  Lexing.pos_fname = "mock_file";
  pos_lnum = 1;
  pos_bol = 0;
  pos_cnum = 1;
}

let dummy_loc = (mock_position, mock_position)

let mk_ident s = { id = s; id_loc = (Lexing.dummy_pos, Lexing.dummy_pos) }
let mk_expr e = { expr_node = e; expr_loc = (Lexing.dummy_pos, Lexing.dummy_pos) }

let mk_stmt st = { stmt_node = st; stmt_loc = (Lexing.dummy_pos, Lexing.dummy_pos) }