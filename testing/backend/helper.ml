open Frontend.Ttree
open Str

let mk_expr e t  = {expr_node = e; expr_ty = t}

let remove_whitespace s = Str.global_replace (Str.regexp "[ \t\n\r]+") "" s