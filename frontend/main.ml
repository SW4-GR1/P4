open Format
open Lexing
open Ptree

let parse_only = ref false
let type_only = ref false


let convert_prog_to_file (prog : Ast.prog) : Ptree.file =
  let convert_stmt_to_ptree_stmt (stmt : Ast.stmt) : Ptree.stmt =
    match stmt with
    | Ast.Ssimple expr -> Ptree.Ssimple expr
    | Ast.Slist stmts -> Ptree.Slist (List.map convert_stmt_to_ptree_stmt stmts)
    | Ast.Sfunc fdec -> 
      let ptree_fdec = {
        Ptree.fun_ty = fdec.Ast.fun_ty;
        Ptree.fun_name = fdec.Ast.fun_name;
        Ptree.fun_args = fdec.Ast.fun_args;
        Ptree.fun_body = convert_stmt_to_ptree_stmt fdec.Ast.fun_body;
      } in
      Ptree.Sfunc ptree_fdec
  in
  let ptree_stmts = List.map convert_stmt_to_ptree_stmt prog.Ast.stmts in
  ("", ptree_stmts)
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