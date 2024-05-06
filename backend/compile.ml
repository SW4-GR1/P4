open Wat
open Frontend.Ttree
open Option


let is_int_type t = match t with
| Tint -> true
| Tlongint -> true
| _ -> false

let compile_unop ty id op =
  match (op : unop) with
  | Inc -> let new_value = binop add ty (get_local id) (int_const ty 1)  in 
    Cat ((set_local id new_value), get_local id)
  | Dec -> let new_value = binop sub ty (get_local id) (int_const ty 1) in
    Cat ((set_local id new_value), get_local id)
  
  
let rec compile_stmt stmt = 
  match stmt with 
  | Ssimple e -> compile_expr e
  | Slist stmts -> compile_stmt_list stmts
  | Sdecl vdec -> let v_e = vdec.var_expr in if is_some v_e 
    then let e' = compile_expr (get v_e) in set_local vdec.var_name e'
    else Nop  (* Vi skal starte med at identificere alle variable deklarationer i en block i toppen*)
  | Sass (id, ty, e) -> set_local id (compile_expr e)
  | Sif (e, s1, s2) -> compile_if e s1 s2
  | Swhile (e, s) -> compile_while e s
  | _ -> failwith "Unsupported statement"

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

and compile_while e s =
  let loop_label = "loop_start" in
  let condition_code = compile_expr e in
  let body_code = compile_stmt s in
  let compiled_while_code =
    let then_block = Command (Cat (S("then"), body_code)) in
    let else_block = Nop in
    let else_construct =
      if s = Slist [] then
        Nop
      else
        Command (Cat (S("else"), else_block))
    in
    Command (Cat (S("block $" ^ loop_label), Cat (condition_code, Cat (S("(if (result i32)"), Cat (S("(then"), Cat (then_block, Cat (S("(br $" ^ loop_label ^ ")"), Cat (S(")"), Cat (else_construct, S(")"))))))))))
  in
  compiled_while_code

and compile_stmt_list stmts = 
  match stmts with
  | [] -> Nop
  | s::[] -> compile_stmt s
  | s::slist -> Cat (compile_stmt s, compile_stmt_list slist)
  
and compile_expr e =
  let ty = e.expr_ty in
  match e.expr_node with
    | Eident(id) -> get_local id
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
      let e1' = compile_expr e1 in
      let e2' = compile_expr e2 in
      match op with
      | Add -> binop add ty e1' e2'
  | Sub -> binop sub ty e1' e2'
  | Div -> let op_str = if is_int_type ty then (div ^ "_s") else div in
    binop op_str ty e1' e2'
  | Mul -> binop mul ty e1' e2'
  | Mod -> binop rem_s ty e1' e2'
  | _ -> failwith "Unsupported binary operator"
  
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
  | _ -> failwith "Unsupported conditional operator"
  
and compile_logical_operators op e1 e2 = 
  let e1' = compile_expr e1 in
  let e2' = compile_expr e2 in
  match (op : log_op) with 
  | And -> wasm_and e1' e2' 
  | Or -> wasm_or e1' e2'
  | _ -> failwith "Unsupported logical operator"

and compile_Fcall id e_list =
  let compiled_args = List.map compile_expr e_list in
  let args_wasm = List.fold_left (fun acc x -> Cat(acc, x)) Nop compiled_args in
  let call_string  = S("call $" ^ id ^ " ") in 
  Command(Cat(call_string, args_wasm))

and compile_declarations stmt_list =
  let table = Hashtbl.create 10 in
  List.iter (function
    | Sdecl { var_name; var_ty } -> Hashtbl.add table var_name var_ty
    | _ -> ()
  ) stmt_list;
  let declarations = Hashtbl.fold (fun name ty acc ->
    let wat_code = decl_local name ty in
    Cat (acc, wat_code)
  ) table Nop in
  declarations

  
let compile ttree = 
  match ttree.stmts with
  | Slist stmts ->
    let compiled_declarations = compile_declarations stmts in
    let compiled_stmts = List.map compile_stmt stmts in
    let compiled_code = List.fold_left (fun acc stmt -> Cat (acc, stmt)) Nop compiled_stmts in
    Wat.module_ (Wat.main_func (Cat(compiled_declarations, Cat(compiled_code, int_const Tint 0))))
| _ -> failwith "Expected a list of statements"