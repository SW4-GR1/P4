open Printf
open Ast
  
  let rec pp_expr = function
  | EConst n -> string_of_int n
  | EIdent x -> x
  | EBinop (op, e1, e2) -> 
    let op_str = match op with
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/" in
    "(" ^ pp_expr e1 ^ " " ^ op_str ^ " " ^ pp_expr e2 ^ ")"
    

let rec pp_stmt = function
  | Slist exprs -> let stmt_list = List.map pp_stmt exprs in
                  String.concat "\n" stmt_list
  | Ssimple e -> pp_expr e

let pp_prog prog =
  pp_stmt prog.main