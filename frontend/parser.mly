%{
    open Ast
%}
// separated_list(separator, type to look for)
%token ADD MUL SUB DIV EOF INC DEC
%token LT GT EQ NEQ LE GE
%token LPAREN RPAREN LBRACE RBRACE COMMA RETURN
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
    f = function_definition {f}
    e = expr { Ssimple(e) }

function_def:
    | type = INT id = IDENT LPAREN params = param_list RPAREN LBRACE stmts = stmt_list RBRACE
      { Function (type, id, params, stmts) }

param_list:
    | { [] }
    | param COMMA param_list { $1 :: $3 }

param:
    | id = IDENT type = int { (id, type) }

// needs more to handle the statement of the function

expr:
    | e1 = expr; o = op; e2 = expr   { EBinop(o, e1, e2) }
    | i = INT                        { EConst(i) }
    | id = IDENT                     { EIdent(id) }


%inline op:
|ADD { Add }
|SUB { Sub }
|MUL { Mul }
|DIV { Div }