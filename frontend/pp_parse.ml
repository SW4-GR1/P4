open Printf
open Ast


let rec pp_types = function
  | Int_ty -> "int"
  | Float_ty -> "float"
  | Long_float_ty -> "long float"
  | Long_int_ty -> "long int"
  | Bool_ty -> "bool" 

let rec pp_expr expr_instance = 
  match expr_instance.expr_node with
  | EConst n -> string_of_int n
  | EFloat fl -> string_of_float fl
  | EBool b -> string_of_bool b
  | EIdent x -> x.id
  | EBinop (op, e1, e2) -> 
      let op_str = match op with
      | Add -> "+"
      | Sub -> "-"
      | Mul -> "*"
      | Div -> "/"
      | Mod -> "%" in
      "(" ^ pp_expr e1 ^ " " ^ op_str ^ " " ^ pp_expr e2 ^ ")"
  | EUnop (id, unop) -> 
      let unop_str = match unop with
      | Inc -> "++"
      | Dec -> "--" in
      "(" ^ id ^ unop_str ^ ")"
  | ECond(op, e1, e2) -> 
      let cond_str = match op with
      | Lt -> "<"
      | Gt -> ">"
      | Eq -> "=="
      | Neq -> "!="
      | Leq -> "<="
      | Geq -> ">=" in
      "(" ^ pp_expr e1 ^ " " ^ cond_str ^ " " ^ pp_expr e2 ^ ")"
  | ELog(op, e1, e2) -> 
      let op_str = match op with
      | And -> "&&"
      | Or -> "||" in
      "(" ^ pp_expr e1 ^ " " ^ op_str ^ " " ^ pp_expr e2 ^ ")"
  | ENot e -> "!" ^ pp_expr e
  | EFcall(id, args) -> 
      let args_str = String.concat ", " (List.map pp_expr args) in
      id.id ^ "( " ^ args_str ^ " )"
  | EArray(e_list) -> 
      let e_list_str = String.concat ", " (List.map pp_expr e_list) in
      "[" ^ e_list_str ^ "]"
  | EArr_lookup(id, e) -> "(" ^ id.id ^ "[" ^ pp_expr e ^ "]" ^ ")"
  | EVector(e_list) -> 
    let e_list_str = String.concat ", " (List.map pp_expr e_list) in
    "{" ^ e_list_str ^ "}"


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

    
let rec pp_array_body body =
  match body with
  | [] -> ""
  | [e] -> pp_expr e
  | e::es -> pp_expr e ^ ", " ^ pp_array_body es

let rec pp_vector_body body =
  match body with
  | [] -> ""
  | [e] -> pp_expr e
  | e::es -> pp_expr e ^ ", " ^ pp_vector_body es

let rec pp_matrix_body body =
  match body with
  | [] -> ""
  | [e] -> pp_array_body e
  | e::es -> pp_array_body e ^ ", " ^ pp_matrix_body es

let pp_a_op = function
  | Assign -> "="
  | Add_assign -> "+="
  | Sub_assign -> "-="
  | Mul_assign -> "*="
  | Div_assign -> "/="


let rec pp_stmt stmt_instance =
match stmt_instance.stmt_node with
  | Slist exprs -> let stmt_list = List.map pp_stmt exprs in
                  String.concat "\n" stmt_list
  | Ssimple e -> let expr_str = pp_expr e in
                     "( "^ (expr_str) ^ " )"
  | Sif (c, e1, e2) ->
    let else_block = match e2.stmt_node with
      | Slist [] -> ""
      | _ -> "else" ^ " { \n" ^ pp_stmt e2 ^ " \n}\n " in
    "if " ^ "( " ^ pp_expr c ^ " )" ^ " { \n" ^ pp_stmt e1 ^ " \n}\n" ^ else_block
  | Sreturn e -> let expr_str = pp_expr e in
                "( return " ^ (expr_str) ^ " )"
 | Sass(ident, a_op, e) -> 
    let a_op_str = pp_a_op a_op in
    "( " ^ ident ^ " " ^ a_op_str ^ " " ^ pp_expr e ^ " ) "
  | Sdecl(vdec) -> 
    let decl_str = "let " ^ pp_types vdec.var_ty ^ " " ^ vdec.var_name.id in
    begin match vdec.var_expr with
    | Some expr -> decl_str ^ " = " ^ pp_expr expr
    | None -> decl_str
    end
  | Sfor(ass, c, reass, s) ->
    let ass_str = match ass.stmt_node with
      (* | Ssimple e -> pp_expr e *)
      | Sdecl vdec -> 
        let decl_str = "let " ^ pp_types vdec.var_ty ^ " " ^ vdec.var_name.id in
        begin match vdec.var_expr with
          | Some expr -> decl_str ^ " = " ^ pp_expr expr
          | None -> decl_str
          end
      | Sass (ident, a_op, e) -> 
        "( " ^ ident ^ " " ^ pp_a_op a_op ^ " " ^ pp_expr e ^ " )"
      | _ -> failwith "Unsupported statement node for for-loop initialization in pp_stmt"
    in
    let cond_str = pp_expr c in
    let reass_str = match reass.stmt_node with
        | Ssimple e -> pp_expr e
        | Sass (ident, a_op, e) -> 
          "( " ^ ident ^ " " ^ pp_a_op a_op ^ " " ^ pp_expr e ^ " )"
        | _ -> failwith "Unsupported statement node for for-loop reassignment in pp_stmt"
      
      
    in
    let stmt_str = pp_stmt s in
    "for (" ^ ass_str ^ "; " ^ cond_str ^ "; " ^ reass_str ^ ") {\n" ^ stmt_str ^ "\n}"
  | Swhile(c, s) -> "while (" ^ pp_expr c ^ ") {\n" ^ pp_stmt s ^ "\n}"
  | Sfunc(func) -> pp_func func 
  | Sarr_decl(t, ident, e1, body_opt) -> 
    let decl_str = "let " ^ pp_types t ^ " " ^ ident.id ^ "[" ^ pp_expr e1 ^ "]" in
    begin match body_opt with
    | Some body -> decl_str ^ " = [" ^ pp_array_body body ^ "]"
    | None -> decl_str
    end
  | Sarr_assign(id, a_op, body) -> let a_op_str = pp_a_op a_op in id ^ " " ^ a_op_str ^ " " ^ "[" ^ pp_array_body body ^ "]" 
  | Sarr_assign_elem(id, e1, a_op, e2) -> let a_op_str = pp_a_op a_op in id ^ "[" ^ pp_expr e1 ^ "]" ^ " " ^ a_op_str ^ " " ^ pp_expr e2 
  | Svec_decl(t, ident, e1, body_opt) -> 
    let decl_str = "let " ^ pp_types t ^"{" ^ pp_expr e1 ^ "}" ^ " " ^ ident.id  in
    begin match body_opt with
    | Some body -> decl_str ^ " = {" ^ pp_array_body body ^ "}"
    | None -> decl_str
    end
  | Svec_assign(id, a_op, body) -> let a_op_str = pp_a_op a_op in id ^ " " ^ a_op_str ^ " " ^ "{" ^ pp_vector_body body ^ "}" 
  | Smat_decl(t, ident, e1, e2, body_opt) ->
    let decl_str = "let " ^ pp_types t ^  "{" ^ pp_expr e1 ^ "}" ^ "{" ^ pp_expr e2 ^ "}" ^ " " ^ ident.id in
    begin match body_opt with
    | Some body -> decl_str ^ " = {" ^ pp_matrix_body body ^ "}"
    | None -> decl_str
    end
  | Sglobal_list(globals) -> let stmt_list = List.map pp_stmt globals in
  String.concat "\n" stmt_list
  | Sfundec_list(funcs) -> let stmt_list = List.map pp_stmt funcs in
  String.concat "\n" stmt_list
  | Sglobal_var(gdec) ->let decl_str = "global " ^ pp_types gdec.gvar_ty ^ " " ^ gdec.gvar_name.id 
   ^ "=" ^ pp_expr gdec.gvar_expr in
   decl_str
  | _ -> failwith "Unexpected case encountered in pp_stmt"

and pp_func func =
  let arg_strs = List.map (fun (type_ident, ident) -> pp_types type_ident ^ " " ^ ident.id) func.args in
  let args_str = String.concat ", " arg_strs in
  let body_str = pp_stmt func.body in
  pp_types func.fun_type ^ " " ^ func.fun_name.id ^ "(" ^ args_str ^ ") {\n" ^
  body_str ^ "\n}\n"
            
let rec pp_export = function
  | Xexport str -> match str with 
  | "" -> ""
  | _ ->  "export " ^ str ^ ";"

let rec pp_export_list exports =
  match exports with
  | [] -> ""
  | [Xexport("")] -> ""
  | _ ->
    let list_str = String.concat "\n"(List.map pp_export exports) in
    "{" ^ list_str ^ "}"
    
let pp_prog prog =
  let exports_str = pp_export_list prog.exports in
  let globals_str = pp_stmt prog.globals in
  let main_str = pp_stmt prog.main in
  exports_str ^ "\n" ^ globals_str ^ "\n" ^ main_str