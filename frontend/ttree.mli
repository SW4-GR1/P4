type ident = string

type ty = 
 | Tint

type binop = Ptree.binop

type assign = ty * ident * epxr

type var_dec = ty * ident

type expr =
  { expr_node: expr_node;
    expr_loc : loc }

and expr_node = 
  | Econst of int
  | Eident of ident
  | EBinop of binop * expr * expr

type stmt = 
 | Ssimple of expr
 | Slist of stmt list



 and fun_dec = {
    fun_ty : ty;
    fun_name : ident;
    fun_args : var_dec;
    fun_body : stmt
 }

 type prog = {
    funs : fun_dec list;
    stmts : stmt
 }
