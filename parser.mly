/* Analyseur syntaxique pour petit-Pyret */

%{
  open Ast

%}

/* Déclaration des tokens */

%token EOF  AND BLOCK CASES ELSE END FALSE FOR FROM FUN IF LAM OR TRUE VAR PLUS MINUS TIMES DIV MOD EQUAL LP RP LSQ RSQ COMMA COLUMN POLARGS
%token <Ast.binop> CMP
%token <int> CINT
%token <string> CSTR IDENT
%token <bool> CBOOL


%left PLUS MINUS TIMES DIV
%nonassoc IF
%nonassoc ELSE

/* Point d'entrée de la grammaire */
%start file

/* Type des valeurs renvoyées par l'analyseur syntaxique */
%type <Ast.file> file

%%

/* Règles de grammaire */

file:
  | s = stmt*; EOF
    { }

block:
  | s = stmt+ {}

stmt:
  | FUN; id,a = fundecl; funbody

fundecl:
  | id = IDENT; 
  

exprparams:
  | p = expr; COMMA; tail = exprparams {p::tail}
  | p = expr; RPAR {[p]}
  | RPAR {[]}

expr:
  | i = CONST {Econst i}
  | i = VAR {Evar i}
  | e1 = expr; PLUS; e2 = expr {Ebinop (Add, e1, e2)}
  | e1 = expr; MINUS; e2 = expr {Ebinop (Sub, e1, e2)}
  | e1 = expr; TIMES; e2 = expr {Ebinop (Mul, e1, e2)}
  | e1 = expr; DIVIDE; e2 = expr {Ebinop (Div, e1, e2)}
  | MINUS; e = expr {Ebinop (Sub, Econst 0, e)}
  | LPAR; e=expr; RPAR {e}

col:
  | BLACK {Turtle.black}
  | WHITE {Turtle.white}
  | RED {Turtle.red}
  | GREEN {Turtle.green}
  | BLUE {Turtle.blue}
;
