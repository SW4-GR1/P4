(* Importing Modules *)
open SymTab
open Ttree

(* Function to convert types to string *)
let type_to_string = function 
  | Tint -> "int"
  | Tfloat -> "float"
  | Tbool -> "bool"

(* Function to convert parse tree types to type tree types*)
let ty_of_pty = function 
  | Ast.Int_ty -> Tint

  (* Function to convert parse location to location *)
let loc_of_ploc (ploc : Ast.loc) : loc = ploc

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
        let vtab_next : varTable = bind ident_name (ty_of_pty pty) vtab in
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
      let ftab_next : funTable = bind ident_name (ty_of_pty pty, arg_types, location) ftab in
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
  | _, _ -> false 

(* Function to check valid typing of operands in expressions *)  
let rec checkExp (ftab : funTable) (vtab : varTable) (exp : Ast.expr) : ty * expr =
  let expr_node = exp.expr_node in
  match expr_node with
  | EConst(c) -> ( Tint,  { expr_node = Econst(c); expr_ty = Tint } )
  | EBool(b)  -> ( Tbool, { expr_node = Ebool(b); expr_ty = Tbool } )
  (* | Eident(i) -> let  *)

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
    
  and checkStmt (ftab : funTable) (vtab : varTable) (stmt : Ast.stmt) : stmt =
    let stmt_node = stmt.stmt_node in 
    match stmt_node with
    | Ssimple(e) -> 
      let (_t, e') = checkExp ftab vtab e in
       Ssimple(e')
      
(*prog -> exports -> stmt*)
let update_parse_tree (ftab: funTable) (vtab : varTable) (p : Ast.prog) : prog =
  let exports = p.exports in 
  let main = p.main in 
    match main.stmt_node with
      |Slist(s) -> let typed_prog = List.map (fun elem -> checkStmt ftab vtab elem) s in
        {stmts = Slist(typed_prog)}
      | _ -> error ("No statement found")


let program p : prog = 
  let ftab = init_fun_table in
  let vtab = init_var_table in
  update_parse_tree ftab vtab p