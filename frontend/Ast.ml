type binop = Add | Mul | Sub | Div | Mod

type unop = Inc | Dec 

type type_ident = Int_ty | Float_ty | Long_int_ty | Long_float_ty| Bool_ty

type cond_binop = Lt | Gt | Eq | Neq | Leq | Geq

type assign_type = Assign | Add_assign | Sub_assign | Mul_assign | Div_assign

type log_op = And | Or

(* type of expression *)

type export = 
  | Xexport of string
  | Xlist of export list


type expr =
  | EBool of bool
  | EConst of int
  | EFloat of float
  | EIdent of string
  | EBinop of binop * expr * expr
  | EUnop of string * unop
  | ECond of cond_binop * expr * expr
  | ELog of log_op * expr * expr
  | ENot of expr
  | EFcall of string * expr list
  | Earray of expr list



(* type of statement *)
type stmt =
  | Ssimple of expr
  | Slist of stmt list
  | Sfunc of func
  | Sif of expr * stmt * stmt
  | Sreturn of expr
  | Sdecl of type_ident * string * expr option 
  | Sass of string * assign_type * expr 
  | Sarr_decl of type_ident * expr * string * expr list option
  | Sarr_assign of string * assign_type * expr list 
  | Sarr_assign_elem of string * expr * assign_type * expr
  | Sfor of stmt * expr * stmt * stmt
  | Swhile of expr * stmt
and func = {
  fun_type : type_ident;
  name : string;
  args : (type_ident * string) list;
  body : stmt; 
}



(* program of list of function declarations, followed by a statement *)
type prog = {
  exports : export list;
  main : stmt;
}