open Printf
open Ttree

let pp_expr_type ty = 
  match ty with 
  | Tint -> "INT"
  |_ -> "AAAAAAAAAA"

let rec pp_expr e = 
  match e.expr_node with 
  | Ebinop (op, e1, e2) -> 
    let op_str = match op with
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%" in
    let ppty = pp_expr_type e.expr_ty in
       "( " ^ ppty ^ "(" ^ pp_expr e1 ^ " " ^ op_str ^ " " ^ pp_expr e2 ^ ")"^ " )"
  | Econst n -> string_of_int n
  | _ -> "BBBBBBBBBBBBBB"


let rec pp_stmt s =
  match s with 
  |Ssimple(e) -> pp_expr e
  |Slist(s) -> let stmt_list = List.map pp_stmt s in
    String.concat "\n" stmt_list

let pp_prog prog =
  (* let exports_str = pp_export_list prog.exports in *)
  let main_str = pp_stmt prog.stmts in
  main_str
  (* exports_str ^ "\n" ^ main_str *)