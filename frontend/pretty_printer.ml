open Printf
open Ast


let rec pp_types = function
  | Int_ty -> "int"
  | Str_ty -> "str"
  
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
  | Ssimple e -> let expr_str = pp_expr e in
                     "( "^ (expr_str) ^ " )"
  | Sif (s, e1, e2) -> 
    let else_block = match e2 with
      | Slist[] -> ""
      | _ -> "else" ^ " { " ^ pp_stmt e2 ^ " } " in
      "if " ^ "( "^  pp_expr s ^  " )" ^  " { " ^ pp_stmt e1 ^ " } " ^ else_block
  | Sreturn e -> let expr_str = pp_expr e in
                "( return " ^ (expr_str) ^ " )"
                       
let rec pp_func_list funcs =
  match funcs with
  | [] -> ""
  | func :: rest ->
    let arg_strs = List.map (fun (type_ident, name) -> pp_types type_ident ^ " " ^ name) func.args in
    let args_str = String.concat ", " arg_strs in
    let body_str = pp_stmt func.body in
    pp_types func.fun_type ^ " " ^ func.name ^ "(" ^ args_str ^ ") {\n" ^
    body_str ^ "\n}\n" ^ pp_func_list rest

let pp_prog prog =
  let fun_decs_str = pp_func_list prog.funDecs in
  let main_str = pp_stmt prog.main in
  fun_decs_str ^ "\n" ^ main_str