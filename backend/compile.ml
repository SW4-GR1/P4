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
  | Sdecl vdec -> Nop (* Vi skal starte med at identificere alle variable deklarationer i en block i toppen*)
  | Sass (id, ty, e) -> set_local id (compile_expr e)
  | Sif (e, s1, s2) -> compile_if e s1 s2
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
    | Econd (op, e1, e2) -> compile_cond op e1 e2
    | Enot e -> let e' = compile_expr e in not ty e'
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

and compile_logical_operators op e1 e2 comepile_expr = 
  let e1' = compile_expr e1 in
  let e2' = compile_expr e2 in
  match (op : log_op) with 
  | And -> 
    Cat (e1', 
      Cat (S "i32.const 1",
        Cat (S "i32.and",
          e2')))         
  | Or ->
    Cat (e1', 
    Cat (S "i32.const 1",
      Cat (S "i32.or",
        e2')))
  | _ -> failwith "Unsupported logical operator"



  let compile ttree = 
    match ttree.stmts with
  | Slist stmts ->
    let compiled_stmts = List.map compile_stmt stmts in
    let compiled_code = List.fold_left (fun acc stmt -> Cat (acc, stmt)) Nop compiled_stmts in
    Wat.module_ (Wat.main_func compiled_code)
  | _ -> failwith "Expected a list of statements"