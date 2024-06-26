type loc = Lexing.position * Lexing.position

type ident = { 
  id : string; 
  id_loc: loc 
}

type binop = Add | Mul | Sub | Div | Mod

type unop = Inc | Dec 

type type_ident = Int_ty | Float_ty | Long_int_ty | Long_float_ty| Bool_ty

type cond_binop = Lt | Gt | Eq | Neq | Leq | Geq

type assign_type = Assign | Add_assign | Sub_assign | Mul_assign | Div_assign

type log_op = And | Or

(* type of expression *)

type export = 
  | Xexport of string



type expr =
  { expr_node: expr_node;
    expr_loc : loc; }
(* type of expression *)

and expr_node =
  | EBool of bool
  | EConst of int
  | EFloat of float
  | EIdent of ident
  | EBinop of binop * expr * expr
  | EUnop of string * unop
  | ECond of cond_binop * expr * expr
  | ELog of log_op * expr * expr
  | ENot of expr
  | EFcall of ident * expr list
  | EArray of expr list
  | EVector of expr list
  | EMatrix of expr list list
  | EArr_lookup of ident * expr
  | ENeg of expr

type vdec = {
  var_ty   : type_ident;
  var_name : ident;
  var_expr : expr option;  (* This field is optional *)
}

type global_vdec = {
  gvar_ty   : type_ident;
  gvar_name : ident;
  gvar_expr : expr;
}


(* 
type adec = {
  arr_ty : type_ident;
  arr_name : ident;
  arr_size : expr;
  arr_expr : expr list option  (* This field is optional *)
} *)

type stmt = 
  { stmt_node: stmt_node;
    stmt_loc : loc }

(* type of statement *)
and stmt_node =
  | Ssimple of expr
  | Slist of stmt list
  | Sfunc of fdec
  | Sif of expr * stmt * stmt
  | Sreturn of expr
  | Sdecl of vdec
  | Sass of string * assign_type * expr 
  | Sarr_decl of type_ident * ident * expr * expr list option
  | Svec_decl of type_ident * ident * expr * expr list option
  | Smat_decl of type_ident * ident * expr * expr * expr list list option
  | Sarr_assign of string * assign_type * expr list 
  | Svec_assign of string * assign_type * expr list
  | Smat_assign of string * assign_type * expr list list
  | Sarr_assign_elem of string * expr * assign_type * expr
  | Svec_assign_elem of string * expr * assign_type * expr
  | Smat_assign_elem of string * expr * expr * assign_type * expr
  | Sfor of stmt * expr * stmt * stmt (*dec * cond * increment * body*)
  | Swhile of expr * stmt
  | Sglobal_var of global_vdec
  | Sglobal_list of stmt list
  | Sfundec_list of stmt list
  (* | Sglobal_arr
  | Sglobal_mat
  | Sglobal_vec *)


(* type of function declaration *)

and arg_dec = type_ident * ident

and fdec = {
  fun_type : type_ident;
  fun_name : ident;
  args : arg_dec list;
  body : stmt; 
}

(* program of list of function declarations, followed by a statement *)
type prog = {
  exports : export list;
  globals : stmt;
  main : stmt;
}