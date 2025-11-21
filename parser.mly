  /* Analyseur syntaxique pour petit-Pyret */

%{
  open Ast

%}

/* Déclaration des tokens */

%token EOF AND BLOCK CASES ELSE END FALSE FOR FROM FUN IF LAM OR TRUE VAR PLUS MINUS TIMES DIV EQUAL LP RP LSQ RSQ COMMA COLON DBLCOLON DEF ARROW LEFT_CHEV RIGHT_CHEV
%token <Ast.binop> CMPblock
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
  | FUN; id = ident; funbody {}
  | VAR; id = IDENT; LP; DBLCOLON; t = typ; RP; EQUAL; bexpr {}
  | id = IDENT; LP; DBLCOLON; t = typ; RP; EQUAL; bexpr {}
  | VAR; id = IDENT; EQUAL; bexpr {}
  | id = IDENT; EQUAL; bexpr {}
  | id = IDENT; DEF; bexpr {}
  | bexpr {}

funbody:
  | LP; p = funpar; ARROW; t = typ; ublock; block; END {}

funpar:
  | RP {[]}
  | p = param; COMMA; f = funpar {p::f}
  | p = param; RP {[p]}

param:
  | id = IDENT; DBLCOLON; t = typ {}

ublock:
  | COLON {}
  | block; COLON {}


typ:
  | id = IDENT {}
  | LP; typestar; typ; RP {}

typestar:
  | ARROW {[]}
  | t = typ; ARROW {[t]}
  | t = typ; COMMA; ts = typestar [t::ts]

bexpr:
  | expr {}
  | bexpr; binop; expr {}

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
