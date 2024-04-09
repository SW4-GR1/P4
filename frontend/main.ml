open Format
open Lexing
open Ast

let parse_only = ref false
let type_only = ref false

(* Define your conversion function *)
let rec convert_expr_to Ast_expr (expr : Ast.expr) : Ast.expr =
match expr with
| Ast.EConst n -> 
  { Ast.expr_node = Ast.Econst n; 
   Ast.expr_loc = loc_of_expr expr }
| Ast.EIdent x -> 
  { Ast.expr_node = Ast.Ecdent x; 
   Ast.expr_loc = loc_of_expr expr }
| Ast.EBinop (op, e1, e2) -> 
  { Ast.expr_node = Ast.Ecinop (op, convert_expr_to Ast_expr e1, convert_expr_to Ast_expr e2); 
   Ast.expr_loc = loc_of_expr expr }
| Ast.ECond (op, e1, e2) -> 
  { Ast.expr_node = Ast.Econd (op, convert_expr_to Ast_expr e1, convert_expr_to Ast_expr e2); 
   Ast.expr_loc = loc_of_expr expr }


let convert_prog_to_file (prog : Ast.prog) : Ast.file =
  let rec convert_stmt_to Ast_stmt (stmt : Ast.stmt) : Ast.stmt =
    match stmt with
    | Ast.Ssimple expr -> 
      { Ast.stmt_node = Ast.Ssimple (convert_expr_to Ast_expr expr); 
       Ast.stmt_loc = loc_of_stmt stmt }
    | Ast.Slist stmts -> 
      { Ast.stmt_node = Ast.Slist (List.map convert_stmt_to Ast_stmt stmts); 
       Ast.stmt_loc = loc_of_stmt stmt }
    | Ast.Sfunc fdec -> 
      let Ast_fdec = {
       Ast.fun_ty = fdec.Ast.fun_ty;
       Ast.fun_name = fdec.Ast.fun_name;
       Ast.fun_args = fdec.Ast.fun_args;
       Ast.fun_body = convert_stmt_to Ast_stmt fdec.Ast.fun_body;
       Ast.fun_loc = loc_of_fdec fdec;
      } in
      { Ast.stmt_node = Ast.Sfunc Ast_fdec; 
       Ast.stmt_loc = loc_of_stmt stmt }
  in
  let Ast_stmts = List.map convert_stmt_to Ast_stmt prog.Ast.stmts in
  ("", Ast_stmts)

(* let () := parse_only true 
   let _ = ! parse_only*)

(* Input and output files *)
let ifile = ref ""
let ofile = ref ""

let set_file f s = f := s


let options =
  ["--parse-only", Arg.Set parse_only,
   "  Only perform syntax analysis of the program";
   "--type-only", Arg.Set type_only,
   " Performs type checking and ends the execution";]

let usage = "usage: main.exe [option] file.yay"


let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l (c-1) c

let () =
  Arg.parse options (set_file ifile) usage;

  if !ifile="" then begin eprintf "Please provide a source file"; exit 1 end;

  if not (Filename.check_suffix !ifile ".yay") then begin
    eprintf "File extension must be .yay";
    Arg.usage options usage;
    exit 1
  end;

  let f = open_in !ifile in

  let buf = Lexing.from_channel f in

  try
    
    let p = Parser.prog Lexer.token buf in
    close_in f;
    
    
    let parsetree = Pretty_printer.pp_prog p in
    if !parse_only then exit 0 else
      let _ = print_endline parsetree in
      let _p = Typechecker.program (convert_prog_to_file p) in 
      if !type_only then exit 0 else 
        print_endline _p
    

  with
    | Lexer.Lexing_error c ->

	localisation (Lexing.lexeme_start_p buf);
	eprintf "Lexical error: %s@." c;
	exit 1
    | Parser.Error ->

	localisation (Lexing.lexeme_start_p buf);
	eprintf "Syntax error@.";
	exit 1