type loc = Lexing.position * Lexing.position

type ident = string

type ty = 
 | Tint
 | Tbool

type binop = Ast.binop

type cond = Ast.cond_binop


type var_dec = ty * ident

type assign = ty * ident * expr

and expr =
{ expr_node: expr_node;
expr_ty : ty }

and expr_node = 
  | Econst of int
  | Ebool of bool
  | Eident of ident
  | Ebinop of binop * expr * expr
  | Econd of cond * expr * expr

type stmt = 
 | Ssimple of expr
 | Slist of stmt list
 | Sfun of fun_dec

 and fun_dec = {
    fun_ty : ty;
    fun_name : ident;
    (* fun_args : var_dec list; *)
    fun_args : ty list;
    (* fun_body : stmt *)
 }

type export = 
  | Xexport

 type prog = {
    (* exports : export list; *)
    stmts : stmt list
 }
