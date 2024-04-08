open SymTab
open Ttree

let type_to_string = function 
  | Tint -> "int"

(* parse tree types to type tree types*)

let ty_of_pty = function 
  | Ast.Int_ty -> Tint

let loc_of_ploc (ploc : Ast.loc) : loc = ploc


exception Error of Ast.loc option * string

let error ?loc msg = raise (Error (loc, msg))

let unbound_variable x = error ("Unbound variable " ^ x)
let unbound_function x = error ("Unbound function " ^ x)


let duplicated_field x = error ("duplicate " ^ x)
let incompatible_types t1 t2 = 
  error ("Incompatible types " ^ type_to_string t1 ^ " and " ^ type_to_string t2)
let bad_arity ?loc p a =
  error ?loc ("bad arity: function p expects " ^
              string_of_int a ^ " arguments")

(* let duplicated_function ?loc new_loc name =
   error ?loc "Duplicate function" ^ name ^ new_loc *)

(* exception Type_error of string *)

type funTable = (ty * ty list * loc) symTab
type varTable = ty symTab


let init_fun_table : funTable = 
  SymTab.fromList [
    ("matMul", (Tmat, [Tmat, Tmat], (Lexing.dummy_pos, Lexing.dummy_pos)));
    ("matAdd", (Tmat, [Tmat, Tmat], (Lexing.dummy_pos, Lexing.dummy_pos)));
    ("matSub", (Tmat, [Tmat, Tnmat], (Lexing.dummy_pos, Lexing.dummy_pos)));
    ("matScale", (Tmat, [Tmat, val_type], (Lexing.dummy_pos, Lexing.dummy_pos)));
    ("matTrans", (Tmat, [Tmat], (Lexing.dummy_pos, Lexing.dummy_pos)));
    ("matDet", (val_type, [Tmat], (Lexing.dummy_pos, Lexing.dummy_pos)));
    ("matEigenVector", (Tarr(Tvec), [Tmat],(Lexing.dummy_pos, Lexing.dummy_pos)));
    ("matEigenValue", (Tarr(Tint), [Tmat], (Lexing.dummy_pos, Lexing.dummy_pos)));
    ("matEigenValue", (Tarr(Tint), [Tmat], (Lexing.dummy_pos, Lexing.dummy_pos)));
    ("matSolveLinearSystem", (Tmat, [Tmat, Tmat], (Lexing.dummy_pos, Lexing.dummy_pos)));
    ("matQR", (Tarr(Tmat), [Tmat], (Lexing.dummy_pos, Lexing.dummy_pos)));
    ("matNormL1", (val_type, [Tmat],(Lexing.dummy_pos, Lexing.dummy_pos)));
    ("matrixD", (Tarr(Tint), [Tmat], (Lexing.dummy_pos, Lexing.dummy_pos)));
    ("sqrt", (val_type, [val_type], (Lexing.dummy_pos, Lexing.dummy_pos)));
    ("pow", (val_type, [val_type, val_type], (Lexing.dummy_pos, Lexing.dummy_pos)));
    ("root", (val_type, [val_type, val_type], (Lexing.dummy_pos, Lexing.dummy_pos)));
    ("max", (val_type, [val_type, val_type],(Lexing.dummy_pos, Lexing.dummy_pos)));
    ("min", (val_type, [val_type, val_type],(Lexing.dummy_pos, Lexing.dummy_pos)));
    ("avg", (val_type, [Tarr(val_type)],(Lexing.dummy_pos, Lexing.dummy_pos)));
  ]


let pp_funtype (args_res : ty list * ty) : string = 
  let (args, res) = args_res in
  match args with
  | [] -> "() -> " ^ type_to_string res
  | args -> (String.concat ", " (List.map type_to_string args))
            ^ " -> " ^ type_to_string res
let check_unique (vl : (Ast.Int_ty * Ast.ident) list) =
  let set = Hashtbl.create 8 in 
  let check (_, {Ast.id = x}) =
    if Hashtbl.mem set x then duplicated_field x;

    Hashtbl.add set x () in
  List.iter check vl

let check_eq_type t1 t2 = match t1, t2 with
  | Tint, Tint -> true
  | _, _ -> false


let rec checkBinop (ftab : funTable) (vtab : varTable) 
  (pos : loc) (t : ty) (e1 : Ast.expr) (e2 : Ast.expr) 
  : ty * expr * expr =
  let (t1, e1') = checkExp ftab vtab e1 in
  let (t2, e2') = checkExp ftab vtab e2 in
  if (t = t1 && t = t2) then (t, e1', e2') 
  else incompatible_types t1 t2 

and checkExp (ftab : funTable) (vtab : varTable) (exp : Ast.expr) : ty * expr =
  let expr_node = exp.expr_node in
  match expr_node with
  | Econst(c) -> ( Tint,  { expr_node = Econst(c); expr_ty = Tint } ) (*2*3+5*6*)
  | Ebool(b)  -> ( Tbool, { expr_node = Ebool(b); expr_ty = Tbool } )

  | Ebinop(op, e1, e2) ->
        let (t1, e1_bin) = checkExp ftab vtab e1 in
        let (t2, e2_bin) = checkExp ftab vtab e2 in
        if check_eq_type t1 t2 
          then (t1, { expr_node = Ebinop(op, e1_bin, e2_bin); expr_ty = t1 } )
          else incompatible_types t1 t2

  | Econd(op, e1, e2) -> 
      let (t1, e1_bin) = checkExp ftab vtab e1 in
      let (t2, e2_bin) = checkExp ftab vtab e2 in
      if check_eq_type t1 t2 
        then (t1, { expr_node = Econd(op, e1_bin, e2_bin); expr_ty = t1 } )
    else incompatible_types t1 t2

(* and checkFunArg (ftab : funTable ) (vtab : vtable) (pos : loc) () *)

  (*
   | Ast.Ebinop (Ast.Blt | Ast.Ble | Ast.Bgt | Ast.Bge |
		  Ast.Badd | Ast.Bsub | Ast.Bmul | Ast.Bdiv |
		  Ast.Band | Ast.Bor as op , e1, e2) ->
      let e1 = expr env e1 in
      let e2 = expr env e2 in
      expected_type Tint e1;
      expected_type Tint e2;
      Ebinop (op, e1, e2), Tint
        *)

let update_fun_table (ftab : funTable) (fundec : Ast.stmt) : funTable =
  let stmtnode = fundec.stmt_node in
  match stmtnode with
  | Sfun(fdec) -> 
    let {Ast.fun_ty = pty; Ast.fun_name = ident; Ast.fun_args = args; _} = fdec in
    let arg_types = List.map (fun (ty, _) -> ty_of_pty ty) args in
    let ident_name = ident.id in
      match lookup ident_name ftab with
      | Some(_, _, prev_loc) -> error "Duplicate function" (*write this out*)
      | None -> let ftab_next : funTable = bind ident_name (ty_of_pty pty, arg_types, loc_of_ploc fundec.stmt_loc) ftab in
        ftab_next
  | _ -> error "Not a function"


  let program (p : Ast.file) : prog = 
    let stmts = snd p in
    let ftab = List.fold_left update_fun_table init_fun_table stmts in
    let ftab_list = toList ftab in 
    let stmts = List.map (fun (ident, (ty, args, loc)) ->
      let fun_dec = { fun_ty = ty; fun_name = ident; fun_args = args} in
      Sfun fun_dec
    ) ftab_list in
    { stmts = stmts }
(* 

let program p = 
  let tdecs = [] in
  let ftab = list.fold_left () initFunTable in
  let typed_decs =  *)


  (*
  let program ~debug p =
  let fl = List.fold_left decl [] p in
  if not (debug || List.exists is_main fl)
  then error "missing main function";
  { funs = fl }   
  *)