%{
    open Ast
%}

%token ADD MUL SUB DIV MOD EOF INC DEC
%token LT GT EQ NEQ LEQ GEQ AND OR NOT
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA DOT RETURN END LET EXPORT
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


%type<Ast.prog> prog

%start prog
%%
//loops
//assignments
prog:
    exports = exports
    s = stmt* 
    EOF
      { { exports = exports; main = Slist s } }
    | s = stmt* EOF { { exports = []; main = Slist s } }
;

exports:
    LBRACE export = export* RBRACE { export }

export:
    |  EXPORT id = IDENT END  { Xexport(id) }

stmt:
    | e = expr END { Ssimple(e) }  
    | c_stmt = control_stmt { c_stmt }
    | decl = declarations { decl }
    | ass = assignment { ass }
    | func = function_def {func}
;

control_stmt:
    | i_stmt = if_stmt {i_stmt}
    | loop = loop_stmt {loop}


declarations:
    | LET d_type = dec_type {d_type}

dec_type:
    | t = ty id = IDENT e = assign_opt END {Sdecl(t, id, e)} // variables
    | d_struc = data_struc_dec {d_struc} // datastructures

assign_opt:
    | ASSIGN e = expr { Some e }
    | { None }


data_struc_dec:
    | t = ty LBRACKET e1 = expr RBRACKET id = IDENT body = array_body_opt END {Sarr_decl(t, e1, id, body)} // array 
    | t = ty LT e1 = expr GT id = IDENT body = vector_body_opt END {Svec_decl(t, e1, id, body)} // vector
    | t = ty LT e1 = expr COMMA e2 =expr GT id = IDENT body = matrix_body_opt END {Smat_decl(t, e1, e2, id, body)}


matrix_body_opt:
    | ASSIGN body = matrix { Some body}
    | { None }
vector_body_opt:
    | ASSIGN body = vector { Some body}
    | { None }

array_body_opt:
    | ASSIGN LBRACKET body = expr_body RBRACKET { Some body}
    | { None }


assignment:
    | ass = assign { ass }
    | data_struc_ass = data_struc_assign {data_struc_ass}
    

assign:
    | id = IDENT ass_op = a_op e = expr END { Sass(id, ass_op, e) }


if_stmt:
    | IF LPAREN c = cond RPAREN LBRACE s = stmt+ RBRACE { Sif(c, Slist s, Slist []) }
    | IF LPAREN c = cond RPAREN LBRACE s1 = stmt+ RBRACE ELSE LBRACE s2 = stmt+ RBRACE { Sif(c, Slist s1, Slist s2) }
;

loop_stmt:
    | f = for_loop { f } 
    | w = while_loop { w }

for_loop:
    | FOR LPAREN
        decl = declarations c = cond END
        ass = assign RPAREN 
        s = block { Sfor(decl, c, ass, Slist s) }

while_loop:
    | WHILE LPAREN c = cond RPAREN 
        s = block { Swhile(c, Slist s) }


block:
    | LBRACE s = stmt+ RBRACE {s}

function_call:
    | id = IDENT LPAREN arg_list = separated_list(COMMA, expr) RPAREN { EFcall(id,arg_list ) }

return_stmt:
    | RETURN e = expr END?{ Sreturn(e) }
;

function_def:
    t = ty id = IDENT 
        LPAREN arg_list = separated_list(COMMA, param) RPAREN
        LBRACE body = func_body RBRACE 
            { Sfunc{fun_type = t; name = id; args = arg_list; body = body} }
;

param:
    | t = ty id = IDENT { (t, id) }
;

func_body:
    | stmts = separated_list(END, stmt) r = return_stmt
        { Slist (stmts @ [r]) }
;

data_struc_assign:
    | id = IDENT ass_op = a_op LBRACKET body = expr_body RBRACKET END {Sarr_assign(id, ass_op, body)}
    | id = IDENT LBRACKET e1 = expr RBRACKET ass_op = a_op e2 = expr END {Sarr_assign_elem(id, e1, ass_op, e2)}
    | id = IDENT ass_op = a_op body = vector END {Svec_assign(id, ass_op, body)}
    | id = IDENT LT e1 = expr GT ass_op = a_op e2 = expr END {Svec_assign_elem(id, e1, ass_op, e2)}
    | id = IDENT ass_op = a_op body = matrix END {Smat_assign(id, ass_op, body)}
    | id = IDENT LT e1 = expr COMMA e2 = expr GT ass_op = a_op e3 = expr END {Smat_assign_elem(id, e1, e2, ass_op, e3)}



expr_body: 
    | e = expr { [e] }
    | e = expr COMMA body = expr_body { e :: body }

expr:
    | e1 = expr; o = op; e2 = expr   { EBinop(o, e1, e2) }
    | id = IDENT u = unop            { EUnop(id, u) }
    | i = INT                        { EConst(i) }
    | fl = FLOAT                     { EFloat(fl)}
    | bl = BOOL                      { EBool(bl)}
    | id = IDENT                     { EIdent(id) }
    | condition = cond               { condition }
    | LPAREN e = expr RPAREN         { e }
    | SUB e = expr %prec uminus      { EBinop(Sub, EConst 0, e) }
    | f_call = function_call         { f_call }
    | LBRACKET body = expr_body RBRACKET { Earray(body) }
    | body = vector { Evector(body)}
    | body = matrix {Ematrix(body)}
;

matrix:
    | LT body = matrix_body GT {body}

matrix_body:
    | v = vector vs = vector_opt {v :: vs}

vector_opt:
    | COMMA v = vector vs = vector_opt {v :: vs}
    | { [] }

vector:
    | LT body = expr_body GT {body}

cond:
    | b = BOOL { EBool(b) }
    | e1 = expr o= l_op e2 = expr  { ELog(o, e1, e2) }
    | e1 = expr o = c_op e2 = expr { ECond(o, e1, e2) }
    | NOT e = expr { ENot(e) }
    
;

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