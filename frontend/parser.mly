%{
    open Ast
%}
// separated_list(separator, type to look for)
%token ADD MUL SUB DIV EOF INC DEC
%token LT GT EQ NEQ LE GE
%token LPAREN RPAREN LBRACE RBRACE COMMA RETURN
%token<int> INT
%token <string> IDENT
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
    s = stmt* EOF { { main = Slist s } }

stmt:
    f = function_def { f }
    | e = expr { Ssimple(e) }
    | IF e = expr LBRACE s = stmt* RBRACE {Sif(e, Slist s, Slist [])}
    | IF e = expr LBRACE s1 = stmt* RBRACE ELSE LBRACE s2 = stmt* RBRACE {Sif(e, Slist s1, Slist s2)}

function_def:
    | t = INT id = IDENT LPAREN params = param_list RPAREN LBRACE stmts = stmt* RBRACE
      { Function (t, id, params, stmts) }

param_list:
    | { [] }
    | param COMMA param_list { $1 :: $3 }

param:
    | id = IDENT t = INT { (id, t) }

// needs more to handle the statement of the function

expr:
    | e1 = expr; o = op; e2 = expr   { EBinop(o, e1, e2) }
    | i = INT                        { EConst(i) }
    | id = IDENT                     { EIdent(id) }
    | SUB e = expr %prec uminus      { EBinop(Sub, EConst 0, e) }


%inline op:
|ADD { Add }
|SUB { Sub }
|MUL { Mul }
|DIV { Div }