open Printf
open Ttree
open Option

let rec pp_expr_type ty = 
  match ty with 
  | Tint -> "INT"
  | Tfloat -> "FLOAT"
  | Tlongint -> "L_INT"
  | Tlongfloat -> "L_FLOAT"
  | Tbool -> "BOOL"
  | Tarr t -> "ARR(" ^ pp_expr_type t ^ ")"
  |_ -> "AAAAAAAAAA"

  let pp_a_op (a_op : assign_type) = 
  match a_op with
  | Assign -> "="
  | Add_assign -> "+="
  | Sub_assign -> "-="
  | Mul_assign -> "*="
  | Div_assign -> "/="
  

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
  | Econst n -> "( " ^ pp_expr_type e.expr_ty ^ " " ^ string_of_int n ^ " )"
  | Efloat f -> "( " ^ pp_expr_type e.expr_ty ^ " " ^ string_of_float f ^ " )"
  | Ebool b -> "( " ^ pp_expr_type e.expr_ty ^ " " ^ string_of_bool b ^ " )"
  | Eident id -> "( " ^ pp_expr_type e.expr_ty ^ " " ^ id ^ " )"
  | Econd(op, e1, e2) -> 
    let op_str = match op with
    | Eq -> "=="
    | Neq -> "!="
    | Lt -> "{"
    | Leq -> "{="
    | Gt -> ">"
    | Geq -> ">=" in
    let ppty = pp_expr_type e.expr_ty in
    "( " ^ ppty ^ "(" ^ pp_expr e1 ^ " " ^ op_str ^ " " ^ pp_expr e2 ^ ")"^ " )"
  | Eunop(ident, op) -> 
    let op_str = match op with
    | Inc -> "++"
    | Dec -> "--" in
    let ppty = pp_expr_type e.expr_ty in
    "( " ^ ppty ^ "(" ^ ident ^ op_str ^")"^ " )"
  | Elog(o, e1, e2) ->
    let op_str = match o with
    | And -> "and"
    | Or -> "or" in
    let ppty = pp_expr_type e.expr_ty in
    "( " ^ ppty ^ "(" ^ pp_expr e1 ^ " " ^ op_str ^ " " ^ pp_expr e2 ^ ")"^ " )"
  | Enot(e) -> "( " ^ pp_expr_type e.expr_ty ^ "(" ^ "!" ^ pp_expr e ^ ")"^ " )"
  | Earray(e_list) -> let expr_list = List.map pp_expr e_list in
     "(" ^ pp_expr_type e.expr_ty ^ "[" ^ String.concat ", " expr_list ^ "]" ^ ")"
  | Earr_lookup(id, e) -> "(" ^ pp_expr_type e.expr_ty ^ " (" ^ id ^ "[" ^ pp_expr e ^ "]" ^ ")" ^ ")"
  | Efcall(id, e_list) -> let expr_list = List.map pp_expr e_list in
    id ^ "(" ^ String.concat ", " expr_list ^ ")"
  | Evector(e_list) -> let expr_list = List.map pp_expr e_list in
    "(" ^ pp_expr_type e.expr_ty ^ "{" ^ String.concat ", " expr_list ^ ">" ^ ")"
 | Ematrix(e_list_list) ->
    let pp_row e_list = List.map pp_expr e_list in
    let expr_rows = List.map pp_row e_list_list in
    "Matrix(" ^ (String.concat ", " (List.map (fun row -> "[" ^ (String.concat ", " row) ^ "]") expr_rows)) ^ ")"

  | _ -> "BBBBBBBBBBBBBB"


let rec pp_stmt s =
  match s with 
  |Ssimple(e) -> pp_expr e
  |Slist(s) -> let stmt_list = List.map pp_stmt s in
    String.concat "\n" stmt_list
  |Sdecl(x) -> if is_some x.var_expr then let init = get x.var_expr in
    "let" ^ " " ^ pp_expr_type x.var_ty ^ " "  ^ x.var_name ^ " = " ^ pp_expr init
    else "let" ^ " " ^ pp_expr_type x.var_ty ^ " " ^ x.var_name
  |Sass(id,_, ass_type, expr) -> 
      let aop = pp_a_op ass_type in 
      let expr_str = pp_expr expr in
    id ^ " " ^ aop ^ " " ^ expr_str
  |Sarr_decl(adec) ->
    let ty = pp_expr_type adec.arr_ty in
      let arr_decl_base = "let" ^ " " ^ ty ^ "["  ^ pp_expr adec.arr_size ^ "]" ^ " " ^ adec.arr_name in
      if is_some adec.arr_expr then let decl_expr_list = List.map pp_expr (get adec.arr_expr) in
        arr_decl_base ^ " " ^ "=" ^ " " ^ "[" ^ String.concat ", " decl_expr_list ^ "]"
      else
        arr_decl_base  
  |Svec_decl(vecdec) ->
    let ty = pp_expr_type vecdec.vec_ty in
      let vec_decl_base = "let" ^ " " ^ ty ^ "{"  ^ pp_expr vecdec.vec_size ^ "}" ^ " " ^ vecdec.vec_name in
      if is_some vecdec.vec_expr then let decl_expr_list = List.map pp_expr (get vecdec.vec_expr) in
        vec_decl_base ^ " " ^ "=" ^ " " ^ "{" ^ String.concat ", " decl_expr_list ^ "}"
      else
        vec_decl_base 

| Smat_decl(mdec) ->
    let ty = pp_expr_type mdec.mat_ty in
    let rows_str = pp_expr mdec.mat_rows in
    let cols_str = pp_expr mdec.mat_cols in
    let mat_decl_base = "let" ^ " " ^ ty ^ "{" ^ rows_str ^ "}" ^ "{"  ^ cols_str ^ "}" ^ " " ^ mdec.mat_name in
    (match mdec.mat_expr with
    | Some(rows) ->  (* 'rows' is a list of list of expressions *)
        let decl_row_list = List.map (fun row -> "[" ^ String.concat ", " (List.map pp_expr row) ^ "]") rows in
        let matrix_str = "{" ^ String.concat ", " decl_row_list ^ "}" in
        mat_decl_base ^ " " ^ "=" ^ " " ^ matrix_str
    | None ->
        mat_decl_base)

  |Sarr_assign(id, ass_type, e_list) ->
    let aop = match ass_type with 
      | Assign -> "="
      | _ -> "HuH" in
      let expr_list = List.map pp_expr e_list in
      id ^ " " ^ aop ^ " [" ^ String.concat ", " expr_list ^ "]"
  |Svec_assign(id, ass_type, e_list) ->
    let aop = match ass_type with 
      | Assign -> "="
      | _ -> "HuH" in
      let expr_list = List.map pp_expr e_list in
      id ^ " " ^ aop ^ " {" ^ String.concat ", " expr_list ^ "}"
  |Sarr_assign_elem(id, expr1, assign_type, expr2) ->
    id ^ " [" ^ pp_expr expr1 ^ "] " ^ pp_a_op assign_type ^ " " ^ pp_expr expr2   
  | Sfor(ass, c, reass, s) ->
    let ass_str = match ass with
      (* | Ssimple e -> pp_expr e *)
      | Sdecl vdec -> 
        let decl_str = "let " ^ pp_expr_type vdec.var_ty ^ " " ^ vdec.var_name in
        begin match vdec.var_expr with
          | Some expr -> decl_str ^ " = " ^ pp_expr expr
          | None -> decl_str
          end
      (* | Sass (ident, a_op, e) -> 
        "( " ^ ident ^ " " ^ pp_a_op a_op ^ " " ^ pp_expr e ^ " )" *)
      | _ -> failwith "Unsupported statement node for for-loop initialization in pp_stmt"
    in
    let cond_str = pp_expr c in
    let reass_str = match reass with
      (* | Ssimple e -> pp_expr e *)
      | Sass (ident,_, a_op, e) -> 
        "( " ^ ident ^ " " ^ pp_a_op a_op ^ " " ^ pp_expr e ^ " )"
      | Ssimple e -> pp_expr e
      | _ -> failwith "Unsupported statement node for for-loop reassignment in pp_stmt"
    in
    let stmt_str = pp_stmt s in
    "for (" ^ ass_str ^ "; " ^ cond_str ^ "; " ^ reass_str ^ ") {\n" ^ stmt_str ^ "\n}"
  | Swhile(c, s) -> "while (" ^ pp_expr c ^ ") {\n" ^ pp_stmt s ^ "\n}"
  | Sif (c, s1, s2) ->
    let else_block = match s2 with
      | Slist [] -> ""
      | _ -> "else" ^ " { \n" ^ pp_stmt s2 ^ " \n}\n " in
    "if " ^ "( " ^ pp_expr c ^ " )" ^ " { \n" ^ pp_stmt s1 ^ " \n}\n" ^ else_block
  | Sreturn(e) ->
    "return " ^ pp_expr e  
  | Sfunc(func) -> 
      let arg_strs = List.map (fun (type_ident, ident) -> pp_expr_type type_ident ^ " " ^ ident) func.fun_args in
      let args_str = String.concat ", " arg_strs in
      let body_str = pp_stmt func.fun_body in
      pp_expr_type func.fun_ty ^ " " ^ func.fun_name ^ "(" ^ args_str ^ ") {\n" ^
      body_str ^ "\n}\n"
  
  | Sglobal_list(globals) -> let stmt_list = List.map pp_stmt globals in
  String.concat "\n" stmt_list
  | Sfundec_list(funcs) -> let stmt_list = List.map pp_stmt funcs in
  String.concat "\n" stmt_list
  |Sglobal_var(gdec) -> 
  "global" ^ " " ^ pp_expr_type gdec.gvar_ty ^ " "  ^ gdec.gvar_name ^ " = " ^ pp_expr gdec.gvar_expr

  let pp_prog prog =
  (* let exports_str = pp_export_list prog.exports in *)
  let globals_str = pp_stmt prog.globals in
  let main_str = pp_stmt prog.stmts in
  globals_str ^ "\n" ^ main_str
