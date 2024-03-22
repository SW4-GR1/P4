%{
    open Ast
%}

%token ADD MUL SUB DIV EOF INC DEC
%token LT GT EQ NEQ LEQ GEQ AND OR NOT
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA DOT RETURN END ASSIGN LET EXPORT
%token<int> INT
%token<float> FLOAT
%token<string> IDENT
%token<bool> BOOL
%token INT_TY STR_TY FLOAT_TY BOOL_TY
%token IF ELSE
%token FOR WHILE

%left ADD SUB
%left MUL DIV
%nonassoc uminus
%nonassoc IF
%nonassoc ELSE


%type<Ast.prog> prog

%start prog
%%
//loops
//assignments
prog:
    exports = export*
    s = stmt* 
    EOF
      { { exports = exports; main = Slist s } }
;

export:
    | LBRACE EXPORT id = IDENT RBRACE { Xexport(id) }

stmt:
    | e = expr END { Ssimple(e) }  
    | i_stmt = if_stmt { i_stmt }
    | ass = assignment { ass }
    | loop = loop_stmt { loop }
    | func = function_def {func}
;

assignment:
    | LET t = ty id = IDENT ASSIGN e = expr END { Sassign(t, id, e) }
    | LET t = ty id = IDENT END { Sdecl(t, id) }
    | reass = reassign { reass }

reassign:
    | id = IDENT ASSIGN e = expr END { Sreass(id, e) }


if_stmt:
    | IF LPAREN c = cond RPAREN LBRACE s = stmt+ RBRACE { Sif(c, Slist s, Slist []) }
    | IF LPAREN c = cond RPAREN LBRACE s1 = stmt+ RBRACE ELSE LBRACE s2 = stmt+ RBRACE { Sif(c, Slist s1, Slist s2) }
;

loop_stmt:
    | f = for_loop { f } 
    | w = while_loop { w }

for_loop:
    | FOR LPAREN
        ass = assignment c = cond END
        reass = reassign RPAREN 
        LBRACE s = stmt+ RBRACE { Sfor(ass, c, reass, Slist s) }

while_loop:
    | WHILE LPAREN c = cond RPAREN 
        LBRACE s = stmt+ RBRACE { Swhile(c, Slist s) }

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

array:
    | LET t = ty LBRACKET e1 = expr RBRACKET i = id ASSIGN LBRACKET array_body* RBRACKET END

array_body: 
    | e = expr { [e] }
    | e = expr COMMA array_body { e :: array_body }

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
;

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
;

%inline unop:
| INC { Inc }
| DEC { Dec }
;

%inline ty:
| INT_TY { Int_ty }
| FLOAT_TY { Float_ty }
| BOOL_TY { Bool_ty }
| STR_TY { Str_ty }
;

%inline l_op:
| AND { And }
| OR { Or }


%inline c_op:
| LT { Lt }
| GT { Gt }
| EQ { Eq }
| NEQ { Neq }
| LEQ { Leq }
| GEQ { Geq }
;