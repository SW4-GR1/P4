open Printf
open Token 

let get_token_list lexbuf =
  let rec work acc =
    match Lexer.token lexbuf with 
    | EOF -> acc 
    | t -> work (t::acc)
  in List.rev (work [])

let pp_token = function
  | ADD -> "ADD"
  | MUL -> "MUL"
  | INT i -> sprintf "INT %d" i
  | EOF -> "EOF"
  | IDENT id -> sprintf "IDENT %s" id 

let main =
  if Array.length Sys.argv < 2 then
    failwith "Please provide a file name as a command line argument"
  else
    let in_channel = open_in Sys.argv.(1) in 
    let lexbuf = Lexing.from_channel in_channel in
    let token_list = get_token_list lexbuf in
    List.map pp_token token_list |> List.iter (printf "%s\n");
    close_in in_channel