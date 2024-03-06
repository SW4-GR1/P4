%{
    open Ast
%}

%token ADD MUL SUB DIV EOF
%token<int> INT
%token <string> IDENT
%token NEWLINE

%left ADD SUB
%left MUL DIV

%type<Ast.prog> prog
%type<Ast.stmt> stmt
%type<Ast.expr> expr

%start prog
%%

prog:
    NEWLINE? s = stmt NEWLINE? EOF { { main = s } }

stmt:
    e = expr { Ssimple(e) }

expr:
    | e1 = expr; ADD; e2 = expr { EBinop(Add, e1, e2) }
    | e1 = expr; SUB; e2 = expr { EBinop(Sub, e1, e2) }
    | e1 = expr; MUL; e2 = expr { EBinop(Mul, e1, e2) }
    | e1 = expr; DIV; e2 = expr { EBinop(Div, e1, e2) }
    | i = INT                   { EConst(i) }
    | id = IDENT                { EIdent(id) }

