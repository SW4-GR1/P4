open Printf
open Ast


let rec pp_types = function
  | Int_ty -> "int"
  | Str_ty -> "str"
  

let rec pp_cond = function
  | EBool b -> string_of_bool b
  | ECond(op, e1, e2) ->
    let op_str = match op with
    | Lt -> "<" in
    "(" ^ pp_expr e1 ^ " " ^ op_str ^ " " ^ pp_expr e2 ^ ")"
    

and pp_expr = function
  | EConst n -> string_of_int n
  | EIdent x -> x
  | EBinop (op, e1, e2) -> 
    let op_str = match op with
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/" in
    "(" ^ pp_expr e1 ^ " " ^ op_str ^ " " ^ pp_expr e2 ^ ")"
  | ECond(op, e1, e2) -> pp_cond (ECond(op, e1, e2)) 
  | EFcall(id, args) -> 
    let args_str = String.concat ", " (List.map pp_expr args) in
    id ^ "( " ^ args_str ^ " )"


let rec pp_stmt = function
  | Slist exprs -> let stmt_list = List.map pp_stmt exprs in
                  String.concat "\n" stmt_list
  | Ssimple e -> let expr_str = pp_expr e in
                     "( "^ (expr_str) ^ " )"
  | Sif (c, e1, e2) -> 
    let else_block = match e2 with
      | Slist[] -> ""
      | _ -> "else" ^ " { \n" ^ pp_stmt e2 ^ " \n}\n " in
      "if " ^ "( "^  pp_cond c ^  " )" ^  " { \n" ^ pp_stmt e1 ^ " \n}\n" ^ else_block
  | Sreturn e -> let expr_str = pp_expr e in
                "( return " ^ (expr_str) ^ " )"
  | Sassign(t, id, e) -> "( let " ^ pp_types t ^ " " ^ id ^ " = " ^ pp_expr e ^ " )"
  | Sreass(id, e) -> "( " ^ id ^ " = " ^ pp_expr e ^ " )"
  | Sfor(ass, c, reass, s) -> 
    let ass_str = pp_stmt ass in
    let cond_str = pp_cond c in
    let reass_str = pp_stmt reass in
    let stmt_str = pp_stmt s in
    "for (" ^ ass_str ^ "; " ^ cond_str ^ "; " ^ reass_str ^ ") {\n" ^ stmt_str ^ "\n}"
  | Swhile(c, s) -> "while (" ^ pp_cond c ^ ") {\n" ^ pp_stmt s ^ "\n}"
            
  
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