%{
    open Ast
%}
// separated_list(separator, type to look for)
%token ADD MUL SUB DIV EOF
%token<int> INT
%token <string> IDENT
%token IF

%left ADD SUB
%left MUL DIV

%type<Ast.prog> prog

%start prog
%%

prog:
    s = stmt* EOF { { main = Slist s } }

stmt:
    e = expr { Ssimple(e) }

expr:
    | e1 = expr; o = op; e2 = expr   { EBinop(o, e1, e2) }
    | i = INT                        { EConst(i) }
    | id = IDENT                     { EIdent(id) }


%inline op:
|ADD { Add }
|SUB { Sub }
|MUL { Mul }
|DIV { Div }