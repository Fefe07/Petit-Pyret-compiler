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
%type <Ast.types> typestar
%type <Ast.param> param
%type <Ast.param list> funpar
%type <Ast.block> ublock
%type <Ast.stmt> stmt_final
%type <Ast.stmt> stmt_init
%type <Ast.funbody> funbody
%type <Ast.param list> funparam
%type <Ast.param> param
%type <Ast.binop> binop
%type <Ast.expr> expr

/* Règles de grammaire */
%%
file:
  | s = stmt*; EOF
    {s}

stmt_final:
  | id = IDENT; DEF; e = bexpr {Saffect (id, e)}
  | b = bexpr {Sexpr b}

block:
  | s1 = stmt*; s2 = stmt_final {s@[s2]}

stmt_init:
  | FUN; id = ident; Funbody (p,t,b) = funbody {Sfun (id,p,t,b)}
  | VAR; id = IDENT; DBLCOLUMN; t = typ; EQUAL; e = bexpr {Svar (id, t, e)}
  | id = IDENT;  DBLCOLUMN; t = typ;  EQUAL; e = bexpr {Svar (id, t, e)}
  | VAR; id = IDENT; EQUAL; e = bexpr {Svar (id, Taundef, e)}
  | id = IDENT; EQUAL; e = bexpr {Svar (id, Taundef, e)}

stmt:
  | s = stmt_init {s}
  | s = stmt_final {s}

funbody:
  | LP; p = funpar; ARROW; t = typ; b = ublock; END {Funbody (p,t,b)}

funpar:
  | RP {[]}
  | p = param; COMMA; f = funpar {p::f}
  | p = param; RP {[p]}

param:
  | id = IDENT; DBLCOLUMN; t = typ {}

ublock:
  | COLUMN ; b = simpleblock; {b}
  | BLOCK; COLUMN ; b = block {b}

simpleblock:
  | s = finblock {[s]}
  | s1 = stmt_init; s2 = simpleblock {s1::s2}

typ:
  | id = IDENT {Tcustom id}
  | LP; t1 = typestar; t2 = typ; RP {Tfun (t1, t2)}

typestar:
  | t = typ; ARROW {t}
  | t = typ; COMMA; ts = typestar {Tproduct (t, ts)}

bexpr:
  | e = expr {e}
  | e1 = bexpr; b = binop; e2 = bexpr {Bexpr (b, e1, e2)}

binop:
  | b = CMP {b}
  | PLUS {Badd}
  | MINUS {Bsub}
  | TIMES {Bmul}
  | DIV {Bdiv}

expr:
  | b = CBOOL {Cst (Cbool b)}
  | i = CINT {Cst (Cint i)}
  | s = CSTR {Cst (Cstr s)}
  | id = IDENT {Var id}
  | LP; b = bexpr; RP {b}
  | BLOCK; COMMA; b = block; END {Eblock b}
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
