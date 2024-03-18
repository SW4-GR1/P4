type binop = Add | Mul | Sub | Div

type type_ident = Int_ty | Str_ty

type expr =
  | EConst of int
  | EIdent of string
  | EBinop of binop * expr * expr

type stmt =
  | Ssimple of expr
  | Slist of stmt list
  | Sif of expr * stmt * stmt
  | Sreturn of expr
  | Sassign of type_ident * string * expr
  | Sreass of string * expr
  
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