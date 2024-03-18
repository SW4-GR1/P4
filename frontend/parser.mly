%{
    open Ast
%}

%token ADD MUL SUB DIV EOF INC DEC
%token LT GT EQ NEQ LE GE
%token LPAREN RPAREN LBRACE RBRACE COMMA RETURN END ASSIGN LET
%token<int> INT
%token<string> IDENT
%token<bool> BOOL
%token INT_TY STR_TY
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
    funcs = function_def*
    s = stmt* 
    EOF
      { { funDecs = funcs; main = Slist s } }
;

stmt:
    | e = expr END { Ssimple(e) }  
    | i_stmt = if_stmt { i_stmt }
    | ass = assignment { ass }
    | loop = loop_stmt { loop }
;

assignment:
    | LET t = ty id = IDENT ASSIGN e = expr END { Sassign(t, id, e) }
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

return_stmt:
    | RETURN e = expr END?{ Sreturn(e) }
;

function_def:
    t = ty id = IDENT 
        LPAREN arg_list = separated_list(COMMA, param) RPAREN
        LBRACE body = func_body RBRACE 
            { {fun_type = t; name = id; args = arg_list; body = body} }
;

param:
    | t = ty id = IDENT { (t, id) }
;

func_body:
    | stmts = separated_list(END, stmt) r = return_stmt
        { Slist (stmts @ [r]) }
;

expr:
    | e1 = expr; o = op; e2 = expr   { EBinop(o, e1, e2) }
    | i = INT                        { EConst(i) }
    | id = IDENT                     { EIdent(id) }
    | condition = cond               { condition }
    | LPAREN e = expr RPAREN         { e }
    | SUB e = expr %prec uminus      { EBinop(Sub, EConst 0, e) }
;

cond:
    | b = BOOL { EBool(b) }
    | e1 = expr o = c_op e2 = expr { ECond(o, e1, e2) }
;

%inline op:
| ADD { Add }
| SUB { Sub }
| MUL { Mul }
| DIV { Div }
;

%inline ty:
| INT_TY { Int_ty }
| STR_TY { Str_ty }
;

%inline c_op:
| LT { Lt }
;