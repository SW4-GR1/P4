%{
    open Ast
%}

%token ADD MUL SUB DIV MOD EOF INC DEC
%token LT GT EQ NEQ LEQ GEQ AND OR NOT
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA DOT RETURN END LET EXPORT GLOBAL
%token ASSIGN ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN
%token<int> INT
%token<float> FLOAT
%token<string> IDENT
%token<bool> BOOL
%token INT_TY FLOAT_TY LONG_INT_TY LONG_FLOAT_TY BOOL_TY
%token IF ELSE
%token FOR WHILE

%left ADD SUB
%left MUL DIV MOD
%nonassoc uminus
%nonassoc IF
%nonassoc ELSE
%nonassoc template_markers LT GT


%type<Ast.prog> prog

%start prog
%%
prog:
    exports = exports
    globals = global*
    s = funcs*
    EOF
       { { exports = exports; globals = {stmt_node = Sglobal_list globals; stmt_loc = $startpos, $endpos};
        main = { 
        stmt_node = Sfundec_list s; stmt_loc = $startpos, $endpos } } }

    | globals = global*
     s = funcs* EOF
        { { exports = [];
            globals = {stmt_node = Sglobal_list globals; stmt_loc = $startpos, $endpos};
            main = { stmt_node = Sfundec_list s; stmt_loc = $startpos, $endpos } } }
;

exports:
    LBRACE exports = export* RBRACE { exports }

export:
    |  EXPORT id = IDENT END  { Xexport(id) }

stmt:
  s = stmt_node
   { { stmt_node = s; stmt_loc = $startpos, $endpos } }
;

stmt_node:
    | e = expr END { Ssimple(e) }  
    | c_stmt = control_stmt { c_stmt }
    | decl = declarations { decl }
    | ass = assignment { ass }
    | ret = return_stmt { ret }
;

control_stmt:
    | i_stmt = if_stmt {i_stmt}
    | loop = loop_stmt {loop}

funcs: 
    | func = function_def { {stmt_node = func; stmt_loc = $startpos, $endpos} }

global:
    dec = global_dec 
    { { stmt_node = dec; stmt_loc = $startpos, $endpos } }

global_dec:
    | GLOBAL t = ty id = ident ASSIGN e = expr END { Sglobal_var({gvar_ty = t; gvar_name = id; gvar_expr = e}) } // variables
    // | GLOBAL t = ty LBRACKET e1 = expr RBRACKET id = ident body = array_body_opt END {Sarr_decl(t, id, e1, body)} // array 
    // | GLOBAL t = ty LBRACE e = expr RBRACE id = ident body = vector_body_opt END {Svec_decl(t, id, e, body)} // vector
    // | GLOBAL t = ty LBRACE e1 = expr RBRACE LBRACE e2 = expr RBRACE id = ident body = matrix_body_opt END {Smat_decl(t, id, e1, e2, body)} // matrix
    

declarations:
    | LET d_type = dec_type {d_type}

dec_type:
  | t = ty id = ident e = assign_opt END { Sdecl({var_ty = t; var_name = id; var_expr = e}) } // variables
  | d_struc = data_struc_dec { d_struc } // data structures

assign_opt:
    | ASSIGN e = expr { Some e }
    | { None }
;

data_struc_dec:
    | t = ty LBRACKET e1 = expr RBRACKET id = ident body = array_body_opt END {Sarr_decl(t, id, e1, body)} // array 
    | t = ty LBRACE e = expr RBRACE id = ident body = vector_body_opt END {Svec_decl(t, id, e, body)} // vector
    | t = ty LBRACE e1 = expr RBRACE LBRACE e2 = expr RBRACE id = ident body = matrix_body_opt END {Smat_decl(t, id, e1, e2, body)} // matrix


matrix_body_opt:
    | ASSIGN body = matrix { Some body}
    | { None }

vector_body_opt:
    | ASSIGN LBRACE body = expr_body RBRACE { Some body }
    | { None }

array_body_opt:
    | ASSIGN LBRACKET body = expr_body RBRACKET { Some body}
    | { None }
;

assignment:
    | ass = assign END { ass }
    | data_struc_ass = data_struc_assign {data_struc_ass}
    

assign:
    | id = IDENT ass_op = a_op e = expr { Sass(id, ass_op, e) }//maybe IDENT, mess around and find out
;

if_stmt:
    | IF LPAREN e = expr RPAREN LBRACE s = stmt+ RBRACE { Sif(e, 
        {stmt_node = Slist s; stmt_loc = $startpos, $endpos},
        {stmt_node = Slist []; stmt_loc = $startpos, $endpos}) }
    | IF LPAREN e = expr RPAREN LBRACE s1 = stmt+ RBRACE ELSE LBRACE s2 = stmt+ RBRACE { 
        Sif(e, 
        {stmt_node = Slist s1; stmt_loc = $startpos, $endpos},
        {stmt_node = Slist s2; stmt_loc = $startpos, $endpos}) }
;

loop_stmt:
    | f = for_loop { f } 
    | w = while_loop { w }
;

for_dec: 
    | LET t = ty id = ident e = assign_opt { Sdecl({var_ty = t; var_name = id; var_expr = e}) }

for_assign:
    | id = IDENT ass_op = a_op e = expr { Sass(id, ass_op, e) }
    | id = IDENT ; u = unop            { Ssimple({expr_node = EUnop(id, u); expr_loc = $startpos, $endpos}) }


for_loop:
    | FOR LPAREN
        decl = for_dec END e = expr END
        ass = for_assign RPAREN 
        s = block { Sfor({stmt_node = decl; stmt_loc = $startpos, $endpos}, e, {stmt_node = ass; stmt_loc = $startpos, $endpos}, 
        { stmt_node = s; stmt_loc = $startpos, $endpos }) }
;

while_loop:
    | WHILE LPAREN e = expr RPAREN 
        s = block { Swhile(e, { stmt_node = s; stmt_loc = $startpos, $endpos }) }
;

block:
    | LBRACE s = stmt+ RBRACE {Slist s}
;

function_call:
    | id = ident LPAREN arg_list = separated_list(COMMA, expr) RPAREN { EFcall(id,arg_list ) }
;

return_stmt:
    | RETURN e = expr END?{ Sreturn(e)} 
;

function_def:
    t = ty id = ident 
        LPAREN arg_list = separated_list(COMMA, param) RPAREN
        LBRACE body = func_body RBRACE 
            { Sfunc{fun_type = t; fun_name = id; args = arg_list; body = body} }
;

param:
    | t = ty id = ident { (t, id) }
;

func_body:
    | stmts = stmt*
        { {stmt_node = Slist (stmts); stmt_loc = $startpos, $endpos } }
;

data_struc_assign:
    | id = IDENT ass_op = a_op LBRACKET body = expr_body RBRACKET END {Sarr_assign(id, ass_op, body)}
    | id = IDENT LBRACKET e1 = expr RBRACKET ass_op = a_op e2 = expr END {Sarr_assign_elem(id, e1, ass_op, e2)}
    | id = IDENT ass_op = a_op LBRACE body = expr_body RBRACE END {Svec_assign(id, ass_op, body)}
    | id = IDENT LBRACE e1 = expr RBRACE ass_op = a_op e2 = expr END {Svec_assign_elem(id, e1, ass_op, e2)}
    | id = IDENT ass_op = a_op body = matrix END {Smat_assign(id, ass_op, body)}
    | id = IDENT LBRACE e1 = expr COMMA e2 = expr RBRACE ass_op = a_op e3 = expr END {Smat_assign_elem(id, e1, e2, ass_op, e3)}



expr_body: 
    | e = expr { [e] }
    | e = expr COMMA body = expr_body { e :: body }

expr:
  e = expr_node
  { { expr_node = e; expr_loc = $startpos, $endpos } }
;

expr_node:
    | e1 = expr; o = op; e2 = expr   { EBinop(o, e1, e2) }
    | i = IDENT; u = unop            { EUnop(i, u) }
    | i = INT                        { EConst(i) }
    | fl = FLOAT                     { EFloat(fl)}
    | bl = BOOL                      { EBool(bl)}
    | id = ident                     { EIdent(id) }
    | condition = cond               { condition.expr_node }
    | LPAREN e = expr RPAREN         { e.expr_node }
    // | SUB e = expr %prec uminus      { EBinop(Sub, {
    //                                     expr_node = EConst(0); expr_loc = $loc}, e) }
    | SUB e = expr %prec uminus      { ENeg(e) }
    | f_call = function_call         { f_call }
    | LBRACKET body = expr_body RBRACKET 
                                     {EArray(body) }
    | body = vector                  {EVector(body)}
    | body = matrix                  {EMatrix(body)}
;

matrix:
    | LBRACE body = matrix_body RBRACE {body}

matrix_body:
    | v = vector vs = vector_opt {v :: vs}

vector_opt:
    | COMMA v = vector vs = vector_opt {v :: vs}
    | { [] }

vector:
    | LBRACE body = expr_body RBRACE %prec template_markers {body}

cond:
    | b = BOOL { {expr_node = EBool(b); expr_loc = $loc } }
    | e1 = expr o= l_op e2 = expr  { {expr_node = ELog(o, e1, e2); expr_loc = $loc } }
    | e1 = expr o = c_op e2 = expr { {expr_node = ECond(o, e1, e2); expr_loc = $loc } }
    | NOT e = expr { {expr_node = ENot(e); expr_loc = $loc } }   
;

ident:
    | id = IDENT { {id = id; id_loc = $startpos, $endpos } }

%inline op:
| ADD { Add }
| SUB { Sub }
| MUL { Mul }
| DIV { Div }
| MOD { Mod }
;

%inline unop:
| INC { Inc }
| DEC { Dec }
;

%inline ty: 
| INT_TY { Int_ty }
| FLOAT_TY { Float_ty }
| LONG_INT_TY { Long_int_ty } 
| LONG_FLOAT_TY { Long_float_ty }
| BOOL_TY { Bool_ty }

;

%inline l_op:
| AND { And }
| OR { Or }

%inline a_op:
| ASSIGN { Assign }
| ADD_ASSIGN { Add_assign }
| SUB_ASSIGN { Sub_assign }
| MUL_ASSIGN { Mul_assign }
| DIV_ASSIGN { Div_assign }

%inline c_op:
| LT { Lt }
| GT { Gt }
| EQ { Eq }
| NEQ { Neq }
| LEQ { Leq }
| GEQ { Geq }
;