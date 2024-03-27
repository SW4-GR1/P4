type loc = Lexing.position * Lexing.position

type ident = { id : string; id_loc: loc }

type ty = 
  | Tint
  | Tbool

(* type unop = Unot | Uminus *)

type binop = 
  Badd | Bsub | Bmul | Bdiv

type cond =
 Cand

type expr =
  { expr_node: expr_node;
    expr_loc : loc }

and expr_node = 
  | Econst of int
  | Ebool of bool
  | Eident of ident
  | Ebinop of binop * expr * expr
  | Econd of cond * expr * expr

type assign_var = ty * ident * expr

type stmt = 
  { stmt_node: stmt_node;
    stmt_loc : loc }

and stmt_node = 
  | Sexpr of expr
  | Slist of stmt list
  | Sfun of fun_dec

and arg_dec = ty * ident

and fun_dec = {
  fun_ty : ty;
  fun_name : ident;
  fun_args : arg_dec list;
  fun_body : stmt
}

type export = 
 | Xexport of string


type file = export list * stmt list