type loc = Lexing.position * Lexing.position

type ident = string 

type ty = 
 | Tint 
 | Tfloat 
 | Tlongint 
 | Tlongfloat
 | Tbool
 | Tmat of ty
 | Tvec of ty
 | Tarr of ty

type binop = Ast.binop

type unop = Ast.unop

type log_op = Ast.log_op

type cond = Ast.cond_binop

type assign_type = Ast.assign_type

type assign = ty * ident * expr

and expr = { 
   expr_node: expr_node;
   expr_ty : ty 
}

and expr_node = 
  | Econst of int
  | Ebool of bool
  | Efloat of float
  | Eident of ident
  | Ebinop of binop * expr * expr
  | Eunop of ident * unop 
  | Econd of cond * expr * expr
  | Elog of log_op * expr * expr
  | Enot of expr
  | Efcall of ident * expr list
  | Earray of expr list
  | Earr_lookup of ident * expr 
  | Evector of expr list
  | Ematrix of expr list list


type vdec = {
   var_ty : ty;
   var_name : ident;
   var_expr : expr option;  (* This field is optional *)
}

type adec = {
   arr_ty : ty;
   arr_name : ident;
   arr_size : expr;
   arr_expr : expr list option;  (* This field is optional *)
}
type vecdec = {
   vec_ty : ty;
   vec_name : ident;
   vec_size : expr;
   vec_expr : expr list option;  (* This field is optional *)
}
type mdec = {
  mat_ty : ty;
  mat_name : ident;
  mat_rows : expr;  
  mat_cols : expr;   
  mat_expr : expr list list option;  (* This field is optional *)
}
type stmt = 
 | Ssimple of expr
 | Slist of stmt list
 | Sfunc of fun_dec
 | Sif of expr * stmt * stmt
 | Sreturn of expr
 | Sdecl of vdec
 | Sass of ident * assign_type * expr
 | Sarr_decl of adec
 | Svec_decl of vecdec
 | Smat_decl of mdec
 | Sarr_assign of ident * assign_type * expr list
 | Svec_assign of ident * assign_type * expr list
 | Smat_assign of ident * assign_type * expr list list
 | Sarr_assign_elem of ident * expr * assign_type * expr
 | Sfor of stmt * expr * stmt * stmt 
 | Swhile of expr * stmt

and fun_arg = ty * ident

and fun_dec = {
   fun_ty : ty;
   fun_name : ident;
   fun_args : fun_arg list;
   fun_body : stmt 
 }


type export = Ast.export

 type prog = {
    exports : export list; 
    stmts : stmt
 }
