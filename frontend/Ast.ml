type binop = Add | Mul | Sub | Div

type unop = Inc | Dec 

type type_ident = Int_ty | Float_ty | Str_ty | Bool_ty

type cond_binop = Lt | Gt | Eq | Neq | Leq | Geq

type log_op = And | Or

(* type of expression *)

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

(* statement *)

type stmt =
  | Ssimple of expr
  | Slist of stmt list
  | Sif of expr * stmt * stmt
  | Sreturn of expr
  | Sassign of type_ident * string * expr
  | Sdecl of type_ident * string
  | Sreass of string * expr
  | Sfor of stmt * expr * stmt * stmt
  | Swhile of expr * stmt
  
(* function declaration *)
type func = {
  fun_type : type_ident;
  name : string;
  args : (type_ident * string) list;
  body : stmt; }


(* program of list of function declarations, followed by a statement *)
type prog = {
  funDecs : func list;
  main : stmt;
}