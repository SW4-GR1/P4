open Frontend.Ttree

let mk_expr e t  = {expr_node = e; expr_ty = t}

let mk_fundec ty name args body = {fun_ty = ty; fun_name = name; fun_args = args; fun_body = body}

let mk_vdec ty name expr_opt = {var_ty = ty; var_name = name; var_expr = expr_opt}

let mk_gvdec ty name expr = {gvar_ty = ty; gvar_name = name; gvar_expr = expr}
