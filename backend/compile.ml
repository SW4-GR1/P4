open Wat
open Frontend.Ttree

let compile_const t c =
match t with
  | Tint -> S (Printf.sprintf "i32.const %d" c)
  | Tlongint -> S (Printf.sprintf "i64.const %d" c)
  | _ -> failwith "Unsupported type"

  let compile_float t f =
  match t with
    | Tfloat -> S (Printf.sprintf "f32.const %f" f)
    | Tlongfloat -> S (Printf.sprintf "f64.const %f" f)
    | _ -> failwith "Unsupported type"

let rec compile_stmt stmt = 
  match stmt with 
  | Ssimple e -> compile_expr e
  | Slist stmts -> compile_stmt_list stmts
  | _ -> failwith "Unsupported statement"


and compile_stmt_list stmts = 
  match stmts with
  | [] -> Nop
  | s::[] -> compile_stmt s
  | s::slist -> Cat (compile_stmt s, compile_stmt_list slist)

and compile_expr e =
  let ty = e.expr_ty in
  match e.expr_node with
    | Econst c -> compile_const ty c 
    | Efloat f -> S (Printf.sprintf "f32.const %f" f)
    | Ebinop (op, e1, e2) -> compile_binop ty op e1 e2 
    | _ -> failwith "Unsupported expression"

and compile_binop ty op e1 e2 =
  let e1' = compile_expr e1 in
  let e2' = compile_expr e2 in
  match op with
  | Add -> add_i32 e1' e2'
  | Sub -> sub_i32 e1' e2'
  | Div -> div_i32 e1' e2'
  | Mul -> mul_i32 e1' e2'
  | Mod -> rem_s_i32 e1' e2'
  | _ -> failwith "Unsupported binary operator"




  let compile ttree = 
    match ttree.stmts with
  | Slist stmts ->
    let compiled_stmts = List.map compile_stmt stmts in
    let compiled_code = List.fold_left (fun acc stmt -> Cat (acc, stmt)) Nop compiled_stmts in
    Wat.module_ (Wat.main_func compiled_code)
  | _ -> failwith "Expected a list of statements"