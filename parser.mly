  /* Analyseur syntaxique pour petit-Pyret */

%{
  open Ast

%}

/* Déclaration des tokens */

%token EOF AND BLOCK CASES ELSE END FALSE FOR FROM FUN IF LAM OR TRUE VAR PLUS MINUS TIMES DIV EQUAL LP RP LSQ RSQ COMMA COLUMN DBLCOLUMN DEF ARROW
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
%type <Ast.block> file
%type <Ast.stmt> stmt
%type <Ast.block> block
%type <Ast.expr> bexpr
%type <Ast.types> typ


/* Règles de grammaire */

file:
  | s = stmt*; EOF
    {s}

block:
  | s = stmt+ {s}

stmt:
  | FUN; id = ident; funbody {}
  | VAR; id = IDENT; DBLCOLUMN; t = typ; EQUAL; e = bexpr {Var (id, t, e)}
  | id = IDENT;  DBLCOLUMN; t = typ;  EQUAL; e = bexpr {Var (id, t, e)}
  | VAR; id = IDENT; EQUAL; e = bexpr {Var (id, Taundef, e)}
  | id = IDENT; EQUAL; e = bexpr {Var (id, Taundef, e)}
  | id = IDENT; DEF; bexpr {}
  | b = bexpr {Sexpr b}

funbody:
  | LP; p = funpar; ARROW; t = typ; ublock; block; END {}

funpar:
  | RP {[]}
  | p = param; COMMA; f = funpar {p::f}
  | p = param; RP {[p]}

param:
  | id = IDENT; DBLCOLUMN; t = typ {}

ublock:
  | COLUMN {}
  | block; COLUMN {}


typ:
  | id = IDENT {}
  | LP; typestar; typ; RP {}

typestar:
  | ARROW {[]}
  | t = typ; ARROW {[t]}
  | t = typ; COMMA; ts = typestar [t::ts]

bexpr:
  | e = expr {e}
  | e1 = bexpr; b = binop; e2 = bexpr {Bexpr (b, e1, e2)}

binop:
  | CMP {}
  | PLUS {}
  | MINUS {}
  | TIMES {}
  | DIV {}

expr:
  | CBOOL {}
  | CINT {}
  | CSTR {}
  | IDENT {}
  | LP; bexpr; RP {}
  | BLOCK; COMMA; block; END {}
  | LAM; funbody {}
  | CASES; LP; typ; RP; bexpr; ublock; branchstar {}
  | caller; LP; bexprstar {}
  | FOR; caller; LP; fromstar; ARROW; typ; ublock; block; END {}

fromstar:
  | RP {}
  | from; RP {}
  | from; COMMA; fromstar {}

bexprstar:
  | RP {}
  | bexpr; RP {}
  | bexpr; COMMA; bexprstar {}

branchstar:
  | END {}
  | branch; branchstar {}

caller:
  | IDENT {}
  | caller; LP; bexprstar {}

from:
  param; FROM; bexpr {}
