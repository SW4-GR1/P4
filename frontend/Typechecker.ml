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

let is_number = function
  | Tint | Tlongint | Tfloat | Tlongfloat -> true
  | _ -> false

let is_bool = function
 | Tbool -> true
 | _ -> false  

(* Function to convert types to string *)
let rec type_to_string = function 
  | Tint -> "int"
  | Tlongint -> "long int"
  | Tfloat -> "float"
  | Tlongfloat -> "long float"
  | Tbool -> "bool"
  | Tarr(t) -> "array of " ^ type_to_string t


(* Exception for errors with optional location and message *)
exception Error of Ast.loc option * string

(* Function to raise an error with optional location and message *)
let error ?loc msg = raise (Error (loc, msg))

(* Error functions for specific error types *)
let unbound_variable x = error ("Unbound variable " ^ x)
let unbound_function x = error ("Unbound function " ^ x)
let duplicated_field x = error ("Duplicate declaration of: " ^ x)
let incompatible_types t1 t2 = 
  error ("Incompatible types " ^ type_to_string t1 ^ " and " ^ type_to_string t2)
let bad_arity ?loc p a =
  error ?loc ("bad arity: function p expects " ^
              string_of_int a ^ " arguments")

(* Type definitions for function and variable tables *)
(* type funTable = (ty * ty list * loc) symTab *)
type funTable = (ty * ty list) symTab
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
(* 
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
    let {Ast.fun_type = pty; Ast.fun_name = ident; Ast.args = args; _} = fdec in
    let arg_types = List.map (fun (ty, _) -> ty_of_pty ty) args in
    let ident_name = ident.id in  
    let location = loc_of_ploc fundec.stmt_loc in
    (match lookup ident_name ftab with
    | Some _ ->
      error ~loc:location ("Duplicate function at " ^ string_of_location location)
    | None ->
      let ftab_next : funTable = SymTab.bind ident_name (ty_of_pty pty, arg_types, location) ftab in
      ftab_next) *)


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
        if is_number t1 then
          if check_eq_type t1 t2 
            then (t1, { expr_node = Ebinop(op, e1_bin, e2_bin); expr_ty = t1 } )
          else incompatible_types t1 t2
        else error("Binary operator should only be used on numbers")

  | ECond(op, e1, e2) -> let (t1, e1_bin) = checkExp ftab vtab e1 in
      let (t2, e2_bin) = checkExp ftab vtab e2 in
      if check_eq_type t1 t2 && is_number t1 then 
      (Tbool, { expr_node = Econd(op, e1_bin, e2_bin); expr_ty = Tbool } )
      else incompatible_types t1 t2

  | EUnop(e1, op) ->
      let (t1, e1_bin) = checkExp ftab vtab e1 in
      if t1 = Tint || t1 = Tlongint 
        then (t1, { expr_node = Eunop(e1_bin, op); expr_ty = t1 })
      else let str_of_op op = match op with 
        | Ast.Inc -> "++"
        | Ast.Dec -> "--" in
          error (str_of_op op ^ " operator applied to a non-numeric type")

  | ELog(op, e1, e2) ->
      let (t1, e1_bin) = checkExp ftab vtab e1 in
      let (t2, e2_bin) = checkExp ftab vtab e2 in
      if t1 = Tbool && t2 = Tbool
        then (Tbool, { expr_node = Elog(op, e1_bin, e2_bin); expr_ty = Tbool })
        else let str_of_lop op = match op with
        | Ast.And -> "and" 
        | Ast.Or -> "or" in
         error (str_of_lop op ^ " operator applied to a non-boolean type")
         
| ENot(e1) ->
      let (t1, e1_bin) = checkExp ftab vtab e1 in
      if t1 = Tbool
        then (Tbool, { expr_node = Enot(e1_bin); expr_ty = Tbool })
        else error ("Not operator applied to a non-boolean type") 
| EArray(e_list) -> 
      let e_list' = List.map (fun e -> checkExp ftab vtab e) e_list in
      let e_types = List.map fst e_list' in
      let all_same = List.fold_left (fun b t -> b && check_eq_type t (List.hd e_types)) true e_types in
      if all_same then
        let ty = List.hd e_types in
        (Tarr(ty), { expr_node = Earray(List.map snd e_list'); expr_ty = Tarr(ty) })
      else error ("Array elements are not of the same type")

| EArr_lookup(id, e1) ->
      let (e1_ty, e1') = checkExp ftab vtab e1 in
      let var_option = SymTab.lookup id vtab in
      if is_some var_option then
        let var_ty = get var_option in
        (match var_ty with
        | Tarr(ty) -> 
          if check_eq_type e1_ty Tint then
            (ty, { expr_node = Earr_lookup(id, e1'); expr_ty = ty })
          else error("Index lookup must evaluate to an integer value")
        | _ -> error ("Variable " ^ id ^ " is not an array."))
      else error ("Variable " ^ id ^ " has not been declared.")
  
(*| EFcall(ident, e1_list) ->
      let id = ident.id in
      let fun_option = SymTab.lookup id ftab in
      if is_some fun_option then
        let (ty, arg_tys, _) = get fun_option in
        let e1_list' = List.map (fun e -> checkExp ftab vtab e) e1_list in
        let expr_list = List.map (fun (_, e) -> e) e1_list' in
        let arg_types = List.map (fun (ty, _) -> ty) e1_list' in
        if arg_types = arg_tys then
          (ty, { expr_node = EFcall(id, e1_list'); expr_ty = ty })
        else bad_arity id (List.length arg_tys)
      else unbound_function id *)

  |EFcall(ident, args) -> 
    let id = ident.id in
    let fun_exists = SymTab.lookup id ftab in 
    if is_some fun_exists then
      let (ret_ty, arg_tys) = get fun_exists in
      let args' = List.map(fun elem -> checkExp ftab vtab elem) args in
      if (List.length args') = (List.length arg_tys) then
        if List.fold_left2 (fun b (ty, _) fun_argty -> b && (check_eq_type ty fun_argty)) true args' arg_tys then
          let exprs = List.map(fun (ty, e) -> e) args' in
            (ret_ty, {expr_node = Efcall(id, exprs); expr_ty = ret_ty})
        else error("Type of arguments in function call do not match")
      else error("Expected " ^ string_of_int(List.length arg_tys) ^ " argument(s), but got " ^ string_of_int(List.length args'))
    else error("Function " ^ id ^" has not been declared")



    (*for each arg check that it evaluates to the correct type 
       according to ftab lookup in the correct order*)
      
  |_ -> assert false
    
  let rec checkStmt (ftab : funTable) (vtab : varTable) (stmt : Ast.stmt) : funTable * varTable * stmt =
    let stmt_node = stmt.stmt_node in 
    match stmt_node with
    | Ssimple(e) -> 
      let (_t, e') = checkExp ftab vtab e in
       ( ftab, vtab, Ssimple(e') )

  
    | Sif(e, tbranch, fbranch) ->
      let (cond_ty, e') = checkExp ftab vtab e in
      let (ftab', vtab', true_branch) = checkStmt ftab vtab tbranch in
      let (ftab'', vtab'', false_branch) = checkStmt ftab vtab fbranch in
      if is_bool cond_ty then
        (ftab, vtab, Sif(e', true_branch, false_branch))
      else error("Condition must evaluate to a boolean value")
    
    | Sfor(dec, cond, inc, body) -> 
      let (ftab', vtab', dec') = checkStmt ftab vtab dec in
      let dec_ident = match dec' with
        | Sdecl(vdec) -> vdec.var_name in
      let (cond_ty, cond') = checkExp ftab' vtab' cond in
      let (_ftab, _vtab, inc') = checkStmt ftab' vtab' inc in
      let (_ftab', _vtab', body') = checkStmt ftab' vtab' body in
      let inc_ident = match inc' with
      | Sass(id,_ , _) -> id in
      if is_bool cond_ty then
        if dec_ident = inc_ident then
          (ftab, vtab, Sfor(dec', cond', inc', body'))
        else error("Increment must be performed on variable " ^ dec_ident)
      else error("Condition must evaluate to a boolean value")

    | Swhile (e, body) ->
      let (cond_ty, e') = checkExp ftab vtab e in
      let (ftab', vtab', body') = checkStmt ftab vtab body in
      if is_bool cond_ty then
        (ftab, vtab, Swhile(e', body'))
      else error("Condition must evaluate to boolean value")

    
    | Sfunc(fdec) -> 
      let f_type = ty_of_pty fdec.fun_type in 
      let (vtab',f_args) = checkArgList ftab vtab fdec.args in
      let arg_type_list = List.map(fun (ty,ident) -> ty) f_args in
      let f_name = fdec.fun_name.id in
      let ftab' = SymTab.bind f_name (f_type, arg_type_list) ftab in
      let body' = checkFunBody ftab' vtab' f_type fdec.body in
      let fdec' = {fun_ty = f_type; fun_name = f_name; fun_args = f_args; fun_body = Slist(body')} in
      (ftab', vtab, Sfunc(fdec'))

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
          ( ftab, vtab, Sass(id, ass_ty, e') )
        else incompatible_types var_ty t
      else error ("Variable " ^ id ^ " has not been declared.")
      
    | Sarr_decl(ty, ident, size, e) ->
        let ty' = ty_of_pty ty in
        let arr_ty = Tarr(ty') in
        let (t_size, size') = checkExp ftab vtab size in
        let name = ident.id in
        if check_eq_type t_size Tint then
        if is_none (SymTab.lookup name vtab) then
          let vtab' = SymTab.bind name arr_ty vtab in
          let expr_option = match e with
          | Some(v) -> let checked_args =  List.map (fun elem -> checkExp ftab vtab elem) v in 
            let arg_types_correct = List.fold_left(fun b (elem_ty, expr) -> b && (check_eq_type elem_ty ty')) true checked_args in
            if arg_types_correct then
              let expr_list = List.map (fun (elem_ty, expr) -> expr) checked_args in
              Some(expr_list)
            else error ("An element in the array is not of the correct type")
          | None -> None in
          let adec' = {arr_ty = ty'; arr_name = name; arr_size = size'; arr_expr = expr_option } in
          ( ftab, vtab', Sarr_decl(adec') )
        else duplicated_field name
        else incompatible_types t_size Tint

    | Sarr_assign(ident, assign_op, expr_list) -> 
      let array_exists = SymTab.lookup ident vtab in
      if is_some array_exists then
        let arr_ty = get array_exists in
        let ty' = match arr_ty with
          | Tarr(t) -> t 
          | _ -> error(ident ^ " is not an array") in
        let checked_args =  List.map (fun elem -> checkExp ftab vtab elem) expr_list in 
        let arg_types_correct = List.fold_left(fun b (elem_ty, expr) -> b && (check_eq_type elem_ty ty')) true checked_args in
          if arg_types_correct then
            let expr_list' = List.map (fun (elem_ty, expr) -> expr) checked_args in
            (ftab, vtab, Sarr_assign(ident, assign_op, expr_list'))
          else error("An element in the array is not of the correct type")
      else error("Array "^ ident ^ " has not been declared")
    
    | Sarr_assign_elem(ident, index, aop, e) -> 
      let arr = SymTab.lookup ident vtab in
      if is_some arr then
        let arr_ty = match (get arr) with 
        | Tarr(ty) -> ty
        | _ -> error("variable " ^ ident ^ " is not an array") in
          let (index_ty, index') = checkExp ftab vtab index in
          let (ty', e') = checkExp ftab vtab e in
          if check_eq_type index_ty Tint then
            if check_eq_type arr_ty ty' then
              (ftab, vtab, Sarr_assign_elem(ident, index', aop, e'))
            else error("The element you are trying to assign is not of the expected type: "^ (type_to_string arr_ty))
          else error("Index lookup must evaluate to an integer value")
      else error("Array " ^ ident ^ " has not been declared")
      

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

and checkArgList (ftab : funTable) (vtab : varTable) (args : Ast.arg_dec list) : (varTable * fun_arg list) =
  match args with 
  | [] -> (vtab, [])
  | (arg_ty, arg_ident)::arglist ->
    let arg_id = arg_ident.id in
    let arg_exists = SymTab.lookup arg_id vtab in
    if is_none arg_exists then
      let arg_ty' = ty_of_pty arg_ty in
      let vtab' = SymTab.bind arg_id arg_ty' vtab in 
      let (vtab_last, arglist') = checkArgList ftab vtab' arglist in
      (vtab_last, [(arg_ty', arg_id)] @ arglist')
    else error("Function argument " ^ arg_ident.id ^ " has been declared elsewhere")

and checkFunBody (ftab : funTable) (vtab : varTable) (ftype : ty) (body : Ast.stmt) : stmt list = 
  let rec checkFunBody_aux (ftab : funTable) (vtab : varTable) (stmts : Ast.stmt list) =  
    match stmts with 
    | [] -> [] 
    | s::slist -> 
      match s.stmt_node with
      | Ast.Sreturn(e) -> 
        let (ty', e') = checkExp ftab vtab e in
        if check_eq_type ty' ftype then
          (* [Sreturn(e')] @ checkFunBody_aux slist *)
          [Sreturn(e')]
        else error("Return type does not match function declaration")
      | _ ->
        let (ftab', vtab', s') = checkStmt ftab vtab s in
          [s'] @ checkFunBody_aux ftab' vtab' slist in

  let body_list = match body.stmt_node with 
  | Slist(s) -> checkFunBody_aux ftab vtab s
  | _ -> error("Function body must be a list of statements") in
  body_list
  
  
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