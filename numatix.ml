open Frontend
open Backend
open Format
open Lexing
open Ast

let parse_only = ref false
let type_only = ref false

(*gci -File -Recurse | Rename-Item -NewName { $_.name -replace "\.(yay|file)$", ".numa" }
   to rename from .yay to .numa in all sub folders*)
(*example of using references*)
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

let usage = "usage: numatix.exe [option] file.numa"


let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l (c-1) c

let () =
  Arg.parse options (set_file ifile) usage;

  if !ifile="" then begin eprintf "Please provide a source file"; exit 1 end;

  if not (Filename.check_suffix !ifile ".numa") then begin
    eprintf "File extension must be .numa";
    Arg.usage options usage;
    exit 1
  end;

  let f = open_in !ifile in

  let buf = Lexing.from_channel f in

  try
    
    let p = Parser.prog Lexer.token buf in
    close_in f;
    
    
    let parsetree = Pp_parse.pp_prog p in
    if !parse_only then exit 0 else
      let _ = print_endline parsetree in
      let _p = Typechecker.program p in
      if !type_only then exit 0 else 
        let typed_tree = Pp_type.pp_prog _p in
        print_endline typed_tree;
        print_endline ("\nTrying to compile " ^ !ifile ^ " to wat");
        let wasm_ast = Compile.compile _p.stmts in
        let base_name = Filename.remove_extension !ifile in
        let ofile = base_name ^ ".wat" in
        Wat.write_wat ofile wasm_ast;
        
        

    
  with
    | Lexer.Lexing_error c ->

	localisation (Lexing.lexeme_start_p buf);
	eprintf "Lexical error: %s@." c;
	exit 1
    | Parser.Error ->

	localisation (Lexing.lexeme_start_p buf);
	eprintf "Syntax error@.";
	exit 1