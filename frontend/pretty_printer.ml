open Printf
open Ast


let rec pp_types = function
  | Int_ty -> "int"
  | Str_ty -> "str"
  | Float_ty -> "float"
  | Bool_ty -> "bool" 
  

let rec pp_cond = function
  | ECond(op, e1, e2) ->
    let op_str = match op with
    | Lt -> "<"
    | Gt -> ">"
    | Eq -> "=="
    | Neq -> "!="
    | Leq -> "<="
    | Geq -> ">="
   in
    "(" ^ pp_expr e1 ^ " " ^ op_str ^ " " ^ pp_expr e2 ^ ")"
  | ELog (l_op, e1, e2) -> 
    let op_str = match l_op with
    | And -> "and"
    | Or -> "or" in
    "(" ^ pp_expr e1 ^ " " ^ op_str ^ " " ^ pp_expr e2 ^ ")"
  | ENot (e) -> "not" ^ " " ^ pp_expr e
  

and pp_expr = function
  | EConst n -> string_of_int n
  | EFloat fl -> string_of_float fl
  | EBool b -> string_of_bool b
  | EIdent x -> x
  | EBinop (op, e1, e2) -> 
    let op_str = match op with
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/" in
    "(" ^ pp_expr e1 ^ " " ^ op_str ^ " " ^ pp_expr e2 ^ ")"
  | EUnop (id, unop) -> 
      let unop_str = match unop with
      | Inc -> "++"
      | Dec -> "--"
      in let id_str = id in
      "(" ^ id_str ^ unop_str ^ ")"
  | ECond(op, e1, e2) -> pp_cond (ECond(op, e1, e2))
  | ELog(op, e1, e2) -> pp_cond (ELog(op, e1, e2))
  | ENot(e) -> pp_cond (ENot(e))
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
  | Sdecl(t, id) -> Printf.sprintf "( let %s %s )" (pp_types t) id
  | Sfor(ass, c, reass, s) -> 
    let ass_str = pp_stmt ass in
    let cond_str = pp_cond c in
    let reass_str = pp_stmt reass in
    let stmt_str = pp_stmt s in
    "for (" ^ ass_str ^ "; " ^ cond_str ^ "; " ^ reass_str ^ ") {\n" ^ stmt_str ^ "\n}"
  | Swhile(c, s) -> "while (" ^ pp_cond c ^ ") {\n" ^ pp_stmt s ^ "\n}"
  | Sfunc(func) -> pp_func func 

and pp_func func =
  let arg_strs = List.map (fun (type_ident, name) -> pp_types type_ident ^ " " ^ name) func.args in
  let args_str = String.concat ", " arg_strs in
  let body_str = pp_stmt func.body in
  pp_types func.fun_type ^ " " ^ func.name ^ "(" ^ args_str ^ ") {\n" ^
  body_str ^ "\n}\n"
            
let rec pp_export = function
  | Xexport str -> "export " ^ str

let rec pp_export_list exports =
  match exports with
  | [] -> ""
  | _ ->
    String.concat "\n" (List.map pp_export exports) 



let pp_prog prog =
  let exports_str = pp_export_list prog.exports in
  let main_str = pp_stmt prog.main in
  exports_str ^ "\n" ^ main_str