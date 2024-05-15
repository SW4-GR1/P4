open Frontend.Ttree
open Str

let mk_expr e t  = {expr_node = e; expr_ty = t}

let mk_fundec ty name args body = {fun_ty = ty; fun_name = name; fun_args = args; fun_body = body}

let remove_whitespace s = Str.global_replace (Str.regexp "[ \t\n\r]+") "" s