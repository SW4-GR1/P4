type loc = Lexing.position * Lexing.position

type ident { id : string; id_loc: loc }

type ty = 
  | Tint

(* type unop = Unot | Uminus *)

type binop = 
  Badd | Bsub | Bmul | Bdiv

type expr =
  { expr_node: expr_node;
    expr_loc : loc }

and expr_node = 
  | Econst of int
  | Eident of ident
  | EBinop of binop * expr * expr

type assign_var = ty * ident * expr

type stmt = 
  { stmt_node: stmt_node;
    stmt_loc : loc }

and stmt_node = 
  | Sexpr of expr
  | Slist of stmt list



type arg_dec = ty * ident

type fun_dec = {
  fun_typ : typ;
  fun_name : ident;
  fun_args : arg_dec list;
  fun_body : stmt
}

type funs_decs = 
  | DFun of fun_dec

type file = fun_decs list * stmt list