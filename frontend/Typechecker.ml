(* Importing Modules *)
open SymTab
open Ttree
open Option

(* Function to convert parse tree types to type tree types*)
let ty_of_pty = function 
  | Ast.Int_ty -> Tint
  | Ast.Long_int_ty -> Tlongint
  | Ast.Float_ty -> Tfloat
  | Ast.Long_float_ty -> Tlongfloat
  | Ast.Bool_ty -> Tbool
  
(* Function to convert types to string *)
let type_to_string = function 
  | Tint -> "int"
  | Tlongint -> "long int"
  | Tfloat -> "float"
  | Tlongfloat -> "long float"
  | Tbool -> "bool"


(* Exception for errors with optional location and message *)
exception Error of Ast.loc option * string

(* Function to raise an error with optional location and message *)
let error ?loc msg = raise (Error (loc, msg))

(* Error functions for specific error types *)
let unbound_variable x = error ("Unbound variable " ^ x)
let unbound_function x = error ("Unbound function " ^ x)
let duplicated_field x = error ("duplicate " ^ x)
let incompatible_types t1 t2 = 
  error ("Incompatible types " ^ type_to_string t1 ^ " and " ^ type_to_string t2)
let bad_arity ?loc p a =
  error ?loc ("bad arity: function p expects " ^
              string_of_int a ^ " arguments")

(* Type definitions for function and variable tables *)
type funTable = (ty * ty list * loc) symTab
type varTable = ty symTab

(* Function to convert location to string *)
let string_of_location ((start_pos, end_pos) : loc) : string =
  let line = start_pos.pos_lnum in
  let start_char = start_pos.pos_cnum - start_pos.pos_bol in
  let end_char = end_pos.pos_cnum - end_pos.pos_bol in
  "line " ^ string_of_int line ^ 
  ", start column " ^ string_of_int start_char ^ 
  ", end column " ^ string_of_int end_char

  
  
    (* Function to convert parse location to location *)
  let loc_of_ploc (ploc : Ast.loc) : loc = ploc

  (* Initial function and variable tables *)
  let init_fun_table : funTable =
    SymTab.fromList [
    (* ("int" (Int_ty, [Int_ty, Int_ty], (0,0))); Example *)
    (* ("matMul" (Mat_ty, [Mat_ty, Mat_ty], (0,0))); *)
    ]
    let init_var_table : varTable = 
      SymTab.fromList []

      (* Function to update variable table with a new variable *)        
  let update_var_table (vtab : varTable) (vardec : Ast.stmt) : varTable =
    let stmtnode = vardec.stmt_node in
    match stmtnode with 
    | Ast.Sdecl(vdec) ->
  let { Ast.var_ty = pty; Ast.var_name = ident; Ast.var_expr = expr_opt } = vdec in
    let ident_name = ident.id in
    (match lookup ident_name vtab with
    | Some _ -> 
        let location = loc_of_ploc vardec.stmt_loc in
        error ~loc:location ("Duplicate variable at " ^ string_of_location location) 
    | None -> 
        let vtab_next : varTable = SymTab.bind ident_name (ty_of_pty pty) vtab in
        vtab_next)
  | _ -> error "Not a variable"


(* Function to update function table with new function *)  
let update_fun_table (ftab : funTable) (fundec : Ast.stmt) : funTable =
  let stmtnode = fundec.stmt_node in
  match stmtnode with
  | Sfunc(fdec) ->
    let {Ast.fun_type = pty; Ast.name = ident; Ast.args = args; _} = fdec in
    let arg_types = List.map (fun (ty, _) -> ty_of_pty ty) args in
    let ident_name = ident in  
    let location = loc_of_ploc fundec.stmt_loc in
    (match lookup ident_name ftab with
    | Some _ ->
      error ~loc:location ("Duplicate function at " ^ string_of_location location)
    | None ->
      let ftab_next : funTable = SymTab.bind ident_name (ty_of_pty pty, arg_types, location) ftab in
      ftab_next)


  (* Function to pretty print function types *)
let pp_funtype (args_res : Ttree.ty list * Ttree.ty) : string = 
  let (args, res) = args_res in
  match args with
  | [] -> "() -> " ^ type_to_string res
  | args -> (String.concat ", " (List.map type_to_string args))
            ^ " -> " ^ type_to_string res

(* Function to check for unique variables *)
let check_unique (vl : (Ast.type_ident * Ast.ident) list) =
  let set = Hashtbl.create 8 in 
  let check (_, {Ast.id = x}) =
    if Hashtbl.mem set x then duplicated_field x;
    Hashtbl.add set x () in
  List.iter check vl

  (* Function to check if two types are equal *)
let check_eq_type t1 t2 = match t1, t2 with
  | Tint, Tint -> true
  | Tlongint, Tlongint -> true
  | Tfloat, Tfloat -> true
  | Tlongfloat, Tlongfloat -> true
  | Tbool, Tbool -> true
  | _, _ -> false 

(* Function to check valid typing of operands in expressions *)  
let rec checkExp (ftab : funTable) (vtab : varTable) (exp : Ast.expr) : ty * expr =
  let expr_node = exp.expr_node in
  match expr_node with
  | EConst(c) -> ( Tint,  { expr_node = Econst(c); expr_ty = Tint } )
  | EBool(b)  -> ( Tbool, { expr_node = Ebool(b); expr_ty = Tbool } )
  | EFloat(f) -> ( Tfloat, { expr_node = Efloat(f); expr_ty = Tfloat } )
  | EIdent(i) -> 
      let id' = i.id in 
      let ident_type = SymTab.lookup id' vtab in
      if is_some ident_type then
        let ty = get ident_type in 
        ( ty, { expr_node = Eident(id'); expr_ty = ty })
      else error("Variable " ^ id' ^ " has not been declared")

  | EBinop(op, e1, e2) ->
        let (t1, e1_bin) = checkExp ftab vtab e1 in
        let (t2, e2_bin) = checkExp ftab vtab e2 in
        if check_eq_type t1 t2 
          then (t1, { expr_node = Ebinop(op, e1_bin, e2_bin); expr_ty = t1 } )
          else incompatible_types t1 t2

  | ECond(op, e1, e2) -> 
      let (t1, e1_bin) = checkExp ftab vtab e1 in
      let (t2, e2_bin) = checkExp ftab vtab e2 in
      if check_eq_type t1 t2 
        then (t1, { expr_node = Econd(op, e1_bin, e2_bin); expr_ty = t1 } )
        else incompatible_types t1 t2
(* 
  | EUnop(op, e1) ->
      let (t1, e1_bin) = checkExp ftab vtab e1 in
      if t1 = Tint || t1 = Tlongint 
        then (t1, { expr_node = Eunop(op, e1_bin); expr_ty = t1 })
        else error (op ^ " operator applied to a non-numeric type")

  | ELog(op, e1, e2) ->
      let (t1, e1_bin) = checkExp ftab vtab e1 in
      let (t2, e2_bin) = checkExp ftab vtab e2 in
      if t1 = Tbool && t2 = Tbool
        then (Tbool, { expr_node = ELog(op, e1_bin, e2_bin); expr_ty = Tbool })
        else error (op ^ " operator applied to a non-boolean type")
   *)
(*| ENot(e1) ->
      let (t1, e1_bin) = checkExp ftab vtab e1 in
      if t1 = Tbool
        then (Tbool, { expr_node = Enot(e1_bin); expr_ty = Tbool })
        else error ("Not operator applied to a non-boolean type") *)
  
(*| EFcall(ident, e1_list) ->
      let id = ident.id in
      let fun_option = SymTab.lookup id ftab in
      if is_some fun_option then
        let (ty, arg_tys, _) = get fun_option in
        let e1_list' = List.map (fun e -> checkExp ftab vtab e) e1_list in
        let arg_types = List.map (fun e -> e.expr_ty) e1_list' in
        if arg_types = arg_tys then
          (ty, { expr_node = EFcall(id, e1_list'); expr_ty = ty })
        else bad_arity id (List.length arg_tys)
      else unbound_function id 
      
  | EArray(ident, e1) ->
      let id = ident.id in
      let e1' = checkExp ftab vtab e1 in
      let var_option = SymTab.lookup id vtab in
      if is_some var_option then
        let var_ty = get var_option in
        (match var_ty with
        | Tarr(ty) -> 
          if check_eq_type e1'.expr_ty Tint then
            (ty, { expr_node = Earray(id, e1'); expr_ty = ty })
          else incompatible_types e1'.expr_ty Tint
        | _ -> error ("Variable " ^ id ^ " is not an array."))
      else error ("Variable " ^ id ^ " has not been declared.") 
     *)
    |_ -> assert false
    
  let rec checkStmt (ftab : funTable) (vtab : varTable) (stmt : Ast.stmt) : funTable * varTable * stmt =
    let stmt_node = stmt.stmt_node in 
    match stmt_node with
    | Ssimple(e) -> 
      let (_t, e') = checkExp ftab vtab e in
       ( ftab, vtab, Ssimple(e') )

       
    | Sdecl(vdec) -> 
      let ty' = ty_of_pty vdec.var_ty in 
      let id' = vdec.var_name.id in
      if is_none (SymTab.lookup id' vtab) then
        let vtab' =  SymTab.bind id' ty' vtab in

        let expr_option = match vdec.var_expr with
          | Some e -> 
            let (t, e') = checkExp ftab vtab e in
            if check_eq_type t ty' then 
              Some e'
            else
              incompatible_types t ty'
          | None -> None in 

        let vdec' = { var_ty = ty'; var_name = id'; var_expr = expr_option } in
        ( ftab, vtab', Sdecl(vdec') )

      else duplicated_field id'
      
    | Sass(ident, ass_ty, e) -> 
      let id = ident in 
      let (t, e') = checkExp ftab vtab e in
      let var_option = SymTab.lookup id vtab in
      if is_some var_option then 
        let var_ty = get var_option in 
        if check_eq_type var_ty t then
          ( ftab, vtab, Sass(id, Assign, e') )
        else incompatible_types var_ty t
      else error ("Variable " ^ id ^ " has not been declared.")
      
      
    (*does not check size of expression or take into account if expr is None*)
    (* | Sarr_decl(ty, size, ident, e) ->
       let ty' = array_ty_of_pty ty in
       let size' = checkExp ftab vtab size in
       let name = ident.id in
       if check_eq_type size' Tint then
        if is_none (SymTab.lookup name vtab) then
          let e' = checkExp ftab vtab e in
          if check_eq_type e'.expr_node ty' then
            let vtab' = SymTab.bind name ty' vtab in
            ( ftab, vtab', Sarr_decl(ty' size', name, e') )
          else incompatible_types e'.expr_node ty'
        else duplicated_field name
       else incompatible_types size' Tint
    
    | Sarr_assign(ident, assign, expr_list) ->
      let exists_option = SymTab.lookup ident.id in
      if is_some exists_option then
        let vtab_ty = get exists_option in
        let arr_ty = match vtab_ty with
          | Tarr(ty) -> ty
          | _        -> error("Array type not found") in
        (*check for each expression that it is same type as array type, 
          check that length is same as size?*)
        let expr_list' = List.map
        
      else ("Array " ^ ident.id ^ " has not been declared.") *)
    | Slist(stmts) -> 
        let stmt_list = checkStmtList ftab vtab stmts in
          ( ftab, vtab, Slist(stmt_list) )
    | _ -> assert false

and checkStmtList (ftab : funTable) (vtab : varTable) (stmts : Ast.stmt list) : stmt list = 
  match stmts with
  | [] -> []
  | s::slist -> 
      let (ftab', vtab', s') = checkStmt ftab vtab s in
       [s'] @ checkStmtList ftab' vtab' slist
  
(*prog -> exports -> stmt*)
let update_parse_tree (ftab : funTable) (vtab : varTable) (p : Ast.prog) : prog =
  let exports = p.exports in 
  let main = p.main in
    let (_ftab, _vtab, typed_prog) = checkStmt ftab vtab main in
    { stmts = typed_prog } 
   

let program p : prog = 
  let ftab = init_fun_table in
  let vtab = init_var_table in
  update_parse_tree ftab vtab p