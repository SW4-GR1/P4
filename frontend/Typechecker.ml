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


(* Function to convert location to string *)
let string_of_location ((start_pos, end_pos) : loc) : string =
  let line = start_pos.pos_lnum in
  let start_char = start_pos.pos_cnum - start_pos.pos_bol in
  let end_char = end_pos.pos_cnum - end_pos.pos_bol in
  "line " ^ string_of_int line ^ 
  ", start column " ^ string_of_int start_char ^ 
  ", end column " ^ string_of_int end_char


(* Exception for errors with optional location and message *)
exception Error of string * string

(* Function to raise an error with optional location and message *)
let error ?loc msg = 
  let loc_str = match loc with
    | Some location -> string_of_location location
    | None -> "Unknown location"
  in
  raise (Error (loc_str, msg))

(* Error functions for specific error types *)
let unbound_variable ?loc x = error ?loc ("Unbound variable " ^ x)
let unbound_function ?loc x = error ?loc ("Unbound function " ^ x)
let duplicated_field ?loc x = error ?loc ("Duplicate declaration of: " ^ x)
let incompatible_types ?loc t1 t2 = 
  error ?loc ("Incompatible types " ^ type_to_string t1 ^ " and " ^ type_to_string t2)
let bad_arity ?loc p a =
  error ?loc ("bad arity: function p expects " ^
              string_of_int a ^ " arguments")

(* Type definitions for function and variable tables *)
(* type funTable = (ty * ty list * loc) symTab *)
type funTable = (ty * ty list) symTab
type varTable = ty symTab


  
  
    (* Function to convert parse location to location *)
  let loc_of_ploc (ploc : Ast.loc) : loc = ploc

  (* Initial function and variable tables *)
  let init_fun_table : funTable =
    SymTab.fromList [
    (* ("int" (Int_ty, [Int_ty, Int_ty], (0,0))); Example *)
    ("sqrt", (Tint, [Tint]));
    ]
    let init_var_table : varTable = 
      SymTab.fromList []

  (* Function to pretty print function types *)
let pp_funtype (args_res : Ttree.ty list * Ttree.ty) : string = 
  let (args, res) = args_res in
  match args with
  | [] -> "() -> " ^ type_to_string res
  | args -> (String.concat ", " (List.map type_to_string args))
            ^ " -> " ^ type_to_string res

(* Function to check for unique variables *)
(* let check_unique (vl : (Ast.type_ident * Ast.ident) list) =
  let set = Hashtbl.create 8 in 
  let check (_, {Ast.id = x}) =
    if Hashtbl.mem set x then duplicated_field x;
    Hashtbl.add set x () in
  List.iter check vl *)

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
  let loc = exp.expr_loc in
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
      else error ~loc ("Variable " ^ id' ^ " has not been declared")

  | EBinop(op, e1, e2) ->
        let (t1, e1_bin) = checkExp ftab vtab e1 in
        let (t2, e2_bin) = checkExp ftab vtab e2 in
        if is_number t1 then
          if check_eq_type t1 t2 
            then (t1, { expr_node = Ebinop(op, e1_bin, e2_bin); expr_ty = t1 } )
          else incompatible_types ~loc t1 t2
        else error ~loc ("Binary operator should only be used on numbers")
  
  | ECond(op, e1, e2) -> let (t1, e1_bin) = checkExp ftab vtab e1 in
      let (t2, e2_bin) = checkExp ftab vtab e2 in
      let bool_allowed = match op with 
        | Ast.Eq -> true
        | Ast.Neq -> true
        | _ -> false in
      if check_eq_type t1 t2 && (is_number t1 || bool_allowed) then 
      (Tbool, { expr_node = Econd(op, e1_bin, e2_bin); expr_ty = Tbool } )
      else incompatible_types ~loc t1 t2

  | EUnop(e1, op) ->
      let (t1, e1_bin) = checkExp ftab vtab e1 in
      if t1 = Tint || t1 = Tlongint 
        then (t1, { expr_node = Eunop(e1_bin, op); expr_ty = t1 })
      else let str_of_op op = match op with 
        | Ast.Inc -> "++"
        | Ast.Dec -> "--" in
          error ~loc (str_of_op op ^ " operator applied to a non-numeric type")

  | ELog(op, e1, e2) ->
      let (t1, e1_bin) = checkExp ftab vtab e1 in
      let (t2, e2_bin) = checkExp ftab vtab e2 in
      if t1 = Tbool && t2 = Tbool
        then (Tbool, { expr_node = Elog(op, e1_bin, e2_bin); expr_ty = Tbool })
        else let str_of_lop op = match op with
        | Ast.And -> "and" 
        | Ast.Or -> "or" in
         error ~loc (str_of_lop op ^ " operator applied to a non-boolean type")
         
| ENot(e1) ->
      let (t1, e1_bin) = checkExp ftab vtab e1 in
      if t1 = Tbool
        then (Tbool, { expr_node = Enot(e1_bin); expr_ty = Tbool })
        else error ~loc ("Not operator applied to a non-boolean type") 
| EArray(e_list) -> 
      let e_list' = List.map (fun e -> checkExp ftab vtab e) e_list in
      let e_types = List.map fst e_list' in
      let all_same = List.fold_left (fun b t -> b && check_eq_type t (List.hd e_types)) true e_types in
      if all_same then
        let ty = List.hd e_types in
        (Tarr(ty), { expr_node = Earray(List.map snd e_list'); expr_ty = Tarr(ty) })
      else error ~loc ("Array elements are not of the same type")

| EArr_lookup(id, e1) ->
      let (e1_ty, e1') = checkExp ftab vtab e1 in
      let var_option = SymTab.lookup id.id vtab in
      if is_some var_option then
        let var_ty = get var_option in
        (match var_ty with
        | Tarr(ty) -> 
          if check_eq_type e1_ty Tint then
            (ty, { expr_node = Earr_lookup(id.id, e1'); expr_ty = ty })
          else  error ~loc ("Index lookup must evaluate to an integer value")
        | _ ->  error ~loc ("Variable " ^ id.id ^ " is not an array."))
      else  error ~loc ("Variable " ^ id.id ^ " has not been declared.")

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
        else error ~loc ("Type of arguments in function call do not match")
      else error ~loc ("Expected " ^ string_of_int(List.length arg_tys) ^ " argument(s), but got " ^ string_of_int(List.length args'))
    else error ~loc ("Function " ^ id ^" has not been declared")

  |EVector(e_list) -> 
    let e_list' = List.map (fun e -> checkExp ftab vtab e) e_list in
    let e_types = List.map fst e_list' in
    let all_same = List.fold_left (fun b t -> b && check_eq_type t (List.hd e_types)) true e_types in
    if all_same then
      let ty = List.hd e_types in
      (Tarr(ty), { expr_node = Evector(List.map snd e_list'); expr_ty = Tarr(ty) })
    else error ~loc ("Vector elements are not of the same type") 
  
  | EMatrix(e_list_list) ->
    let check_row e_list = 
        let e_list' = List.map (fun e -> checkExp ftab vtab e) e_list in
        let e_types = List.map fst e_list' in
        let all_same = List.fold_left (fun b t -> b && check_eq_type t (List.hd e_types)) true e_types in
        if all_same then
            (Tarr(List.hd e_types), List.map snd e_list')
        else error ~loc ("Matrix row elements are not of the same type")
    in
    let row_results = List.map check_row e_list_list in
    let row_types = List.map fst row_results in
    let all_same_row_type = List.fold_left (fun b t -> b && check_eq_type t (List.hd row_types)) true row_types in
    if all_same_row_type then
        let ty = Tmat(List.hd row_types) in
        (ty, { expr_node = Ematrix(List.map snd row_results); expr_ty = ty })
    else error ~loc ("Matrix rows are not of the same type")

      
  |_ -> assert false
    
  let rec checkStmt (ftab : funTable) (vtab : varTable) (stmt : Ast.stmt) : funTable * varTable * stmt =
    let loc = stmt.stmt_loc in
    let stmt_node = stmt.stmt_node in 
    match stmt_node with
    | Sreturn(e) ->
         error ~loc ("Return statement outside of function")
    | Ssimple(e) -> 
      let (_t, e') = checkExp ftab vtab e in
       ( ftab, vtab, Ssimple(e') )

  
    | Sif(e, tbranch, fbranch) ->
      let (cond_ty, e') = checkExp ftab vtab e in
      let (ftab', vtab', true_branch) = checkStmt ftab vtab tbranch in
      let (ftab'', vtab'', false_branch) = checkStmt ftab vtab fbranch in
      if is_bool cond_ty then
        (ftab, vtab, Sif(e', true_branch, false_branch))
      else error ~loc ("Condition must evaluate to a boolean value")
 (*   
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
        else error ~loc ("Increment must be performed on variable " ^ dec_ident)
      else error ~loc ("Condition must evaluate to a boolean value")
*)
    | Swhile (e, body) ->
      let (cond_ty, e') = checkExp ftab vtab e in
      let (ftab', vtab', body') = checkStmt ftab vtab body in
      if is_bool cond_ty then
        (ftab, vtab, Swhile(e', body'))
      else error ~loc ("Condition must evaluate to boolean value")

    
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
              incompatible_types ~loc t ty'
          | None -> None in 

        let vdec' = { var_ty = ty'; var_name = id'; var_expr = expr_option } in
        ( ftab, vtab', Sdecl(vdec') )

      else duplicated_field ~loc id'
      
    | Sass(ident, ass_ty, e) -> 
      let id = ident in 
      let (t, e') = checkExp ftab vtab e in
      let var_option = SymTab.lookup id vtab in
      if is_some var_option then 
        let var_ty = get var_option in 
        if check_eq_type var_ty t then
          ( ftab, vtab, Sass(id, ass_ty, e') )
        else incompatible_types  ~loc var_ty t
      else error ~loc ("Variable " ^ id ^ " has not been declared.")
      
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
            else error ~loc ("An element in the array is not of the correct type")
          | None -> None in
          let adec' = {arr_ty = ty'; arr_name = name; arr_size = size'; arr_expr = expr_option } in
          ( ftab, vtab', Sarr_decl(adec') )
        else duplicated_field ~loc name
        else incompatible_types ~loc t_size Tint

    | Sarr_assign(ident, assign_op, expr_list) -> 
      let array_exists = SymTab.lookup ident vtab in
      if is_some array_exists then
        let arr_ty = get array_exists in
        let ty' = match arr_ty with
          | Tarr(t) -> t 
          | _ -> error ~loc (ident ^ " is not an array") in
        let checked_args =  List.map (fun elem -> checkExp ftab vtab elem) expr_list in 
        let arg_types_correct = List.fold_left(fun b (elem_ty, expr) -> b && (check_eq_type elem_ty ty')) true checked_args in
          if arg_types_correct then
            let expr_list' = List.map (fun (elem_ty, expr) -> expr) checked_args in
            (ftab, vtab, Sarr_assign(ident, assign_op, expr_list'))
          else error ~loc ("An element in the array is not of the correct type")
      else error ~loc ("Array "^ ident ^ " has not been declared")
    
    | Sarr_assign_elem(ident, index, aop, e) -> 
      let arr = SymTab.lookup ident vtab in
      if is_some arr then
        let arr_ty = match (get arr) with 
        | Tarr(ty) -> ty
        | _ -> error ~loc ("variable " ^ ident ^ " is not an array") in
          let (index_ty, index') = checkExp ftab vtab index in
          let (ty', e') = checkExp ftab vtab e in
          if check_eq_type index_ty Tint then
            if check_eq_type arr_ty ty' then
              (ftab, vtab, Sarr_assign_elem(ident, index', aop, e'))
            else error ~loc ("The element you are trying to assign is not of the expected type: "^ (type_to_string arr_ty))
          else error ~loc ("Index lookup must evaluate to an integer value")
      else error ~loc ("Array " ^ ident ^ " has not been declared")
      
    | Svec_decl(ty, ident, size, e) ->
      let ty' = ty_of_pty ty in
      let vec_ty = Tvec(ty') in
      let (t_size, size') = checkExp ftab vtab size in
      let name = ident.id in
      if check_eq_type t_size Tint then
      if is_none (SymTab.lookup name vtab) then
        let vtab' = SymTab.bind name vec_ty vtab in
        let expr_option = match e with
        | Some(v) -> let checked_args =  List.map (fun elem -> checkExp ftab vtab elem) v in 
          let arg_types_correct = List.fold_left(fun b (elem_ty, expr) -> b && (check_eq_type elem_ty ty')) true checked_args in
          if arg_types_correct then
            let expr_list = List.map (fun (elem_ty, expr) -> expr) checked_args in
            Some(expr_list)
          else error ~loc ("An element in the vector is not of the correct type")
        | None -> None in
        let vecdec' = {vec_ty = ty'; vec_name = name; vec_size = size'; vec_expr = expr_option } in
        ( ftab, vtab', Svec_decl(vecdec') )
      else duplicated_field ~loc name
      else incompatible_types ~loc t_size Tint

    | Smat_decl(ty, ident, dim1, dim2, e) ->
    let ty' = ty_of_pty ty in
    let mat_ty = Tmat(ty') in
    let (t_dim1, dim1') = checkExp ftab vtab dim1 in
    let (t_dim2, dim2') = checkExp ftab vtab dim2 in
    let name = ident.id in
    if check_eq_type t_dim1 Tint && check_eq_type t_dim2 Tint then
        if is_none (SymTab.lookup name vtab) then
            let vtab' = SymTab.bind name mat_ty vtab in
            let expr_option = match e with
            | Some(vv) ->
                let checked_rows = List.map (fun row -> 
                    List.map (fun elem -> checkExp ftab vtab elem) row) vv in
                let row_types_correct = List.map (fun row ->
                    List.fold_left (fun b (elem_ty, expr) -> b && (check_eq_type elem_ty ty')) true row) checked_rows in
                if List.for_all (fun x -> x) row_types_correct then
                    let expr_rows = List.map (fun row -> List.map snd row) checked_rows in
                    Some(expr_rows)
                else error ~loc ("An element in the matrix is not of the correct type")
            | None -> None
            in
            let matdec' = { mat_ty = ty'; mat_name = name; mat_rows = dim1'; mat_cols = dim2'; mat_expr = expr_option } in
            (ftab, vtab', Smat_decl(matdec'))
        else duplicated_field ~loc name
    else incompatible_types ~loc t_dim1 Tint


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
    else let loc = arg_ident.id_loc in error ~loc ("Function argument " ^ arg_ident.id ^ " has been declared elsewhere")

and checkFunBody (ftab : funTable) (vtab : varTable) (ftype : ty) (body : Ast.stmt) : stmt list = 
  let rec checkFunBody_aux (ftab : funTable) (vtab : varTable) (stmts : Ast.stmt list) =  
    match stmts with 
    | [] -> [] 
    | s::slist -> 
      let loc = s.stmt_loc in
      match s.stmt_node with
      | Ast.Sreturn(e) -> 
        let (ty', e') = checkExp ftab vtab e in
        if check_eq_type ty' ftype then
          [Sreturn(e')]
        else error ~loc ("Return type does not match function declaration")
      | Ast.Sif(e, tbranch, fbranch) ->
        let (ty', e') = checkExp ftab vtab e in
        if not (check_eq_type ty' Tbool) then
          error ~loc ("Condition in if statement must be of type bool");
        let tbranch', fbranch' = 
          match tbranch.stmt_node, fbranch.stmt_node with
          | Slist t, Slist f -> 
            checkFunBody_aux ftab vtab t, checkFunBody_aux ftab vtab f
          | _ -> 
            error ~loc ("Expected Slist in branches of Sif")
      in
      [Sif(e', Slist(tbranch'), Slist(fbranch'))] @ checkFunBody_aux ftab vtab slist (* Vi ville miste alle asignments af variables her right?*)
      | Ast.Swhile (e, body) ->
        let (cond_ty, e') = checkExp ftab vtab e in
        if not (check_eq_type cond_ty Tbool) then
          error ~loc ("Condition in while loop must be of type bool");
        let body' = 
          match body.stmt_node with
          | Slist b -> checkFunBody_aux ftab vtab b
          | _ -> error ~loc ("Expected Slist in body of Swhile")
        in
        [Swhile(e', Slist(body'))] @ checkFunBody_aux ftab vtab slist
      (*| Ast.Sfor(dec, cond, inc, body) ->
        let (ftab', vtab', dec') = checkStmt ftab vtab dec in
        let dec_ident = match dec' with
          | Sdecl(vdec) -> vdec.var_name in
        let (cond_ty, cond') = checkExp ftab' vtab' cond in
        let (_ftab, _vtab, inc') = checkStmt ftab' vtab' inc in
        let body' = 
          match body.stmt_node with
          | Slist b -> checkFunBody_aux ftab' vtab' b
          | _ -> error ~loc ("Expected Slist in body of Sfor")
        in
        let inc_ident = match inc' with
        | Sass(id,_ , _) -> id in
        if is_bool cond_ty then
          if dec_ident = inc_ident then
            [Sfor(dec', cond', inc', Slist(body'))] @ checkFunBody_aux ftab' vtab' slist
          else error ~loc ("Increment must be performed on variable " ^ dec_ident)
        else error ~loc ("Condition must evaluate to a boolean value")*)
      | _ ->
        let (ftab', vtab', s') = checkStmt ftab vtab s in
          [s'] @ checkFunBody_aux ftab' vtab' slist in

  let body_list = let loc = body.stmt_loc in match body.stmt_node with 
  | Slist(s) -> 
    let has_return = List.exists (fun (stmt : Ast.stmt) -> 
      match stmt.stmt_node with
      | Sreturn _ -> true
      | _ -> false
    ) s in
    if not has_return then
      error ~loc ("Function is missing a return statement");
    checkFunBody_aux ftab vtab s
  | _ -> error ~loc ("Function body must be a list of statements") in
  body_list
  
  
(*prog -> exports -> stmt*)
let update_parse_tree (ftab : funTable) (vtab : varTable) (p : Ast.prog) : prog =
  let exports = p.exports in 
  let main = p.main in
    let (_ftab, _vtab, typed_prog) = checkStmt ftab vtab main in
    List.iter (fun export -> match export with
      | Ast.Xexport(fname) -> if is_none (SymTab.lookup fname ftab) then
        unbound_function fname) exports;
    { stmts = typed_prog } 
   

let program p : prog = 
  let ftab = init_fun_table in
  let vtab = init_var_table in
  update_parse_tree ftab vtab p