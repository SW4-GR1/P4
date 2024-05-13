open Wat
open Frontend.Ttree







open Option

let globals_map = Hashtbl.create 20

let check_global id = Hashtbl.mem globals_map id

let reversed_condition (op : cond) : cond = match op with
| Eq -> Neq
| Neq -> Eq
| Lt -> Geq
| Leq -> Gt
| Gt -> Leq
| Geq -> Lt
let is_int_type t = match t with
| Tint -> true
| Tlongint -> true
| _ -> false

let compile_unop ty id op =
  let is_global = check_global id in
  match (op : unop) with
  | Inc -> let new_value = binop add ty (get_var id is_global) (int_const ty 1)  in 
    Cat ((set_var id new_value is_global), get_var id is_global)
  | Dec -> let new_value = binop sub ty (get_var id is_global) (int_const ty 1) in
    Cat ((set_var id new_value is_global), get_var id is_global)
  
  
let rec compile_stmt stmt = 
  match stmt with 
  | Ssimple e -> 
    (match e.expr_node with
    | Eunop (_, _) -> compile_expr e
    | _ -> Command(Cat(S("drop"), compile_expr e)))
  | Slist stmts -> compile_stmt_list stmts
  | Sfunc fdec -> compile_func fdec
  | Sreturn e -> let e' = compile_expr e in 
    return e'
    
  | Sdecl vdec -> let v_e = vdec.var_expr in if is_some v_e 
    then 
    let t_opt = vdec.var_ty in let e' = compile_expr ~t_opt (get v_e) 
    in set_local vdec.var_name e'
    else Nop  (* Vi skal starte med at identificere alle variable deklarationer i en block i toppen*)
  | Sass (id, id_ty, ass_op, e) -> 
    let is_global = check_global id in 
    let t_opt = id_ty in
    (match ass_op with 
    | Assign -> set_var id (compile_expr ~t_opt e) is_global
    | _ ->  let e_id = {expr_node = Eident(id); expr_ty = id_ty} in
            let (op : binop) = match ass_op with 
              | Add_assign -> Add
              | Sub_assign -> Sub
              | Mul_assign -> Mul
              | Div_assign -> Div 
              | _ -> failwith("How did we end up here")
          in let e' = {expr_node = Ebinop(op, e_id, e); expr_ty = id_ty} in 
          set_var id (compile_expr e') is_global)
      

    
  | Sif (e, s1, s2) -> compile_if e s1 s2
  | Sfor (init, cond, update, s) -> compile_for init cond update s
  | Swhile (e, s) -> compile_while e s
  | _ -> Nop

and compile_if e s1 s2 = 
  let e' = compile_expr e in
  let then_block = compile_stmt s1 in
  let then_construct = Command (Cat (S("then"), then_block)) in
 let else_construct = 
  if 
    match s2 with 
    | Slist [] -> false
    | _ -> true
    then
    let else_block = compile_stmt s2 in
    Command (Cat (S("else"), else_block))
  else 
    Nop
  in
  Command (Cat (S("if"), Cat (e', Cat (then_construct, else_construct))))

and compile_for init cond update s = 
  let init' = compile_stmt init in 
    let cond' = match cond.expr_node with 
    | Ebool b -> bool_const b
    | Econd (op, e1, e2) -> let br_op = reversed_condition op in 
      compile_cond br_op e1 e2
    | _ -> failwith "Unsupported condition"
    in 
    let update' = compile_stmt update in
    let block = S("block") in
    let loop = S("loop") in
    Command (Cat(block, 
      Cat(init', 
        Command(Cat(loop,
          Cat(Command(Cat(S("br_if 1"),cond')), 
            Cat(compile_stmt s, 
              Cat(update', Command(S("br 0"))))))))))
and compile_func fdec = 
    let { fun_ty; fun_name; fun_args; fun_body } = fdec in
      let f_sig = func_sig fun_ty fun_name fun_args in
      let body = (match fun_body with 
        | Slist stmts ->
            let compiled_declarations = compile_declarations stmts in
            let compiled_stmts = List.map compile_stmt stmts in
            let compiled_code = List.fold_left (fun acc stmt -> Cat (acc, stmt)) Nop compiled_stmts in
            Cat (compiled_declarations, compiled_code)
        | _ -> failwith "Expected a list of statements")
      in let indented_body = String.split_on_char '\n' (to_string body) |> List.map ((^) "\t") |> String.concat "\n" in
      Command (Cat(f_sig, S(indented_body)))
       
    

and compile_while e s =
  let cond' = match e.expr_node with 
    | Ebool b -> bool_const b
    | Econd (op, e1, e2) -> let br_op = reversed_condition op in 
      compile_cond br_op e1 e2
    | _ -> failwith "Unsupported condition"
    in 
  let block = S("block") in
  let loop = S("loop") in
  Command (Cat(block, 
    Command(Cat(loop,
      Cat(Command(Cat(S("br_if 1"),cond')), 
        Cat(compile_stmt s, Command(S("br 0"))))))))



and compile_stmt_list stmts = 
  match stmts with
  | [] -> Nop
  | s::[] -> compile_stmt s
  | s::slist -> Cat (compile_stmt s, compile_stmt_list slist)
  
and compile_expr ?t_opt e  =
  let ty = (match t_opt with
  | Some t -> t
  | None -> e.expr_ty)
  in
  match e.expr_node with
    | Eident(id) -> let extend = e.expr_ty != ty in let is_global = check_global id in
      if extend then extend_i64 (get_var id is_global)
      else get_var id is_global
    | Econst c -> int_const ty c
    | Ebool b -> bool_const b
    | Efloat f -> float_const ty f
    | Ebinop (op, e1, e2) -> compile_binop op ty e1 e2 
    | Eunop (id, op) -> compile_unop ty id op
    | Elog (op, e1, e2) -> compile_logical_operators op e1 e2
    | Econd (op, e1, e2) -> compile_cond op e1 e2
    | Enot e -> let e' = compile_expr e in not ty e'
    | Efcall (id, e_list) -> compile_Fcall id e_list
    | _ -> failwith "Unsupported expression"

    and compile_binop op ty e1 e2 =
      let t_opt = ty in
      let e1' = compile_expr ~t_opt e1 in
      let e2' = compile_expr ~t_opt e2 in
      match op with
      | Add -> binop add ty e1' e2'
  | Sub -> binop sub ty e1' e2'
  | Div -> let op_str = if is_int_type ty then (div ^ "_s") else div in
    binop op_str ty e1' e2'
  | Mul -> binop mul ty e1' e2'
  | Mod -> binop rem_s ty e1' e2'
  
  
  and compile_cond op e1 e2 =
  let ty = e1.expr_ty in
  let e1' = compile_expr e1 in
  let e2' = compile_expr e2 in
  match op with
  | Eq -> cond eq ty e1' e2'
  | Neq -> cond ne ty e1' e2'
  | Lt -> let op_str = if is_int_type ty then (lt ^ "_s") else lt in
  cond op_str ty e1' e2'
  | Leq -> let op_str = if is_int_type ty then (le ^ "_s") else le in
  cond op_str ty e1' e2'
  | Gt ->  let op_str = if is_int_type ty then (gt ^ "_s") else gt in
    cond op_str ty e1' e2'
    | Geq -> let op_str = if is_int_type ty then (ge ^ "_s") else ge in
    cond op_str ty e1' e2'
 
  
and compile_logical_operators op e1 e2 = 
  let e1' = compile_expr e1 in
  let e2' = compile_expr e2 in
  match (op : log_op) with 
  | And -> wasm_and e1' e2' 
  | Or -> wasm_or e1' e2'
  

and compile_Fcall id e_list =
  let compiled_args = List.map compile_expr e_list in
  let args_wasm = List.fold_left (fun acc x -> Cat(acc, x)) Nop compiled_args in
  let call_string  = S("call $" ^ id ^ " ") in 
  Command(Cat(call_string, args_wasm))

and compile_declarations stmt_list =
  let table = Hashtbl.create 10 in
  let rec find_declarations stmt_list =
    List.iter (function
      | Sdecl { var_name; var_ty; _} -> Hashtbl.add table var_name var_ty
      | Sif(_, Slist tbranch, Slist fbranch) -> 
        find_declarations tbranch;
        find_declarations fbranch
      | Swhile(_, Slist body) -> find_declarations body
      | Sfor(local, _, _, Slist body) -> (match local with
        | Sdecl { var_name; var_ty; _} -> Hashtbl.add table var_name var_ty
        | _ -> ());  
        find_declarations body
      | _ -> ()
    ) stmt_list
  in
  find_declarations stmt_list;
  let declarations = Hashtbl.fold (fun name ty acc ->
    let wat_code = decl_local name ty in
    Cat (acc, wat_code)
  ) table Nop in
  declarations

  
let compile ttree = 
  let exports = ttree.exports in 
  let stmts = (match ttree.stmts with
  | Sfundec_list stmts -> stmts 
  | _ -> failwith "Expected a lists of statements ahhh") in
  let globals = (match ttree.globals with
  | Sglobal_list stmts -> stmts
  | _ -> failwith "Somethings wrong with da globals") in
  List.iter (function 
  | Sglobal_var(dec) -> Hashtbl.add globals_map dec.gvar_name dec.gvar_ty
  | _ -> failwith "Unexpected statement in globals list") globals;
  let compiled_globals = List.map (function
  | Sglobal_var {gvar_ty = t; gvar_name = n; gvar_expr = e } -> global_var n t (compile_expr e)
  | _ -> failwith "Unexpected statement in globals list") globals in
  let globals_code = List.fold_left  (fun acc global  -> Cat (acc, global)) Nop compiled_globals in
  let compiled_exports = List.map ((function Xexport f_name -> export_func f_name) : export -> _) exports in
  let exports_code = List.fold_left  (fun acc export  -> Cat (acc, export)) Nop compiled_exports in
  let compiled_stmts = List.map compile_stmt stmts in
  let stmts_code = List.fold_left (fun acc stmt -> Cat (acc, stmt)) Nop compiled_stmts in
  Wat.module_ (Cat(Cat(exports_code, globals_code), stmts_code))
 