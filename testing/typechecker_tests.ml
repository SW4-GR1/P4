open Frontend
open Frontend.Ast
open OUnit2


(* hjælpe funktioner til mocking af Ast *)
let mk_ident s = { id = s; id_loc = (Lexing.dummy_pos, Lexing.dummy_pos) }
let mk_expr e = { expr_node = e; expr_loc = (Lexing.dummy_pos, Lexing.dummy_pos) }

let mk_vtab = SymTab.fromList [] (* empty variable table *)
let mk_ftab = SymTab.fromList [] (* empty function table *)
 
let remove_whitespace s = Str.global_replace (Str.regexp "[ \t\n\r]+") "" s

let test_typecheck_add _test_ctxt = 
  (* Create an AST representing the expression "1 + 1" *)
  let ast = mk_expr (EBinop (Add, mk_expr (EConst 1), mk_expr (EConst 1))) in
  let expected_out = "(INT((INT1)+(INT1)))" in
  (* Typecheck the AST *)
  let ftab = mk_ftab in 
  let vtab = mk_vtab in
  let (ty, expr) = Typechecker.checkExp ftab vtab ast in
  let ttree_string = remove_whitespace (Pp_type.pp_expr expr) in
  print_endline ttree_string;
  assert_equal Ttree.Tint ty;
  assert_equal expected_out ttree_string

let suite = "TypecheckerTests" >::: [
    "test_typecheck:add" >:: test_typecheck_add;
    
  ] 

let _ = run_test_tt_main suite 
