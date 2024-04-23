open Printf
open Ttree
open Option

let pp_expr_type ty = 
  match ty with 
  | Tint -> "INT"
  | Tbool -> "BOOL"
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
  | Ebool b -> string_of_bool b
  | Eident id -> id
  | Econd(op, e1, e2) -> 
    let op_str = match op with
    | Eq -> "=="
    | Neq -> "!="
    | Lt -> "<"
    | Leq -> "<="
    | Gt -> ">"
    | Geq -> ">=" in
    let ppty = pp_expr_type e.expr_ty in
    "( " ^ ppty ^ "(" ^ pp_expr e1 ^ " " ^ op_str ^ " " ^ pp_expr e2 ^ ")"^ " )"
  | Eunop(e, op) -> 
    let op_str = match op with
    | Inc -> "++"
    | Dec -> "--" in
    let ppty = pp_expr_type e.expr_ty in
    "( " ^ ppty ^ "(" ^ pp_expr e ^ op_str ^")"^ " )"
  | Elog(o, e1, e2) ->
    let op_str = match o with
    | And -> "and"
    | Or -> "or" in
    let ppty = pp_expr_type e.expr_ty in
    "( " ^ ppty ^ "(" ^ pp_expr e1 ^ " " ^ op_str ^ " " ^ pp_expr e2 ^ ")"^ " )"
  | Enot e -> "( " ^ pp_expr_type e.expr_ty ^ "(" ^ "!" ^ pp_expr e ^ ")"^ " )"
  | Efcall(id, args) -> 
    let pp_fcall id expr_list =
      let expr_strs = List.map (fun e -> pp_expr_type e.expr_ty ^ " " ^ pp_expr e) expr_list in
      id ^ "(" ^ String.concat ", " expr_strs ^ ")"
    in
    pp_fcall id args
  | _ -> "BBBBBBBBBBBBBB"


let rec pp_stmt s =
  match s with 
  |Ssimple(e) -> pp_expr e
  |Slist(s) -> let stmt_list = List.map pp_stmt s in
    String.concat "\n" stmt_list
  |Sdecl(x) -> if is_some x.var_expr then let init = get x.var_expr in
    "let" ^ " " ^ pp_expr_type x.var_ty ^ " "  ^ x.var_name ^ " = " ^ pp_expr init
    else "let" ^ " " ^ pp_expr_type x.var_ty ^ " " ^ x.var_name
let pp_prog prog =
  (* let exports_str = pp_export_list prog.exports in *)
  let main_str = pp_stmt prog.stmts in
  main_str
