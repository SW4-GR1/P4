%{
    open Ast
%}
// separated_list(separator, type to look for)
%token ADD MUL SUB DIV EOF INC DEC
%token LT GT EQ NEQ LE GE
%token LPAREN RPAREN LBRACE RBRACE COMMA RETURN END
%token<int> INT
%token <string> IDENT
%token INT_TY STR_TY
%token IF ELSE

%left ADD SUB
%left MUL DIV
%nonassoc uminus
%nonassoc IF
%nonassoc ELSE


%type<Ast.prog> prog

%start prog
%%

prog:
    funcs = function_def*
    s = stmt* 
    EOF
      { { funDecs = funcs; main = Slist s } }
;

stmt:
    | e = expr END { Ssimple(e) }  
    | IF e = expr LBRACE s = stmt RBRACE {Sif(e, s, Slist [])}
    | IF e = expr LBRACE s1 = stmt RBRACE ELSE LBRACE s2 = stmt RBRACE {Sif(e, s1, s2)}
;

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
    | LPAREN e = expr RPAREN         { e }
    | SUB e = expr %prec uminus      { EBinop(Sub, EConst 0, e) }
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