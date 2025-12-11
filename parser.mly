  /* Analyseur syntaxique pour petit-Pyret */

%{
  open Ast

%}

/* Déclaration des tokens */

%token LEFT_CHEV RIGHT_CHEV EOF BLOCK CASES ELSE END FOR FROM FUN IF LAM VAR EQUAL LP RP COMMA COLON DBLCOLON DEF ARROW DOUBLEARROW PIPE
%token <Ast.binop> BINOP
%token <int> CINT
%token <string> CSTR IDENT
%token <bool> CBOOL

/* Point d'entrée de la grammaire */
%start file

/* Type des valeurs renvoyées par l'analyseur syntaxique */

%type <Ast.block> file
%type <Ast.stmt> stmt
%type <Ast.stmt list> list(stmt)
%type <Ast.stmt> stmt_init
%type <Ast.stmt> stmt_final

%type <Ast.expr> expr
%type <Ast.expr> bexpr
%type <Ast.expr list> bexprstar
%type <Ast.ident list> identstar

%type <Ast.block> block
%type <Ast.block> simpleblock
%type <Ast.block> ublock

%type <Ast.funbody> funbody
%type <Ast.param list> funpar
%type <Ast.param> param
%type <Ast.caller> caller
%type <Ast.expr list> call_arg

%type <Ast.expr> ifblock
%type <Ast.expr> elif
%type <Ast.expr> simpleelif

%type <Ast.branch list> branches
%type <Ast.branch> branch
%type <Ast.branch> simplebranch
%type <Ast.branch list> branchstar
%type <Ast.branch list> simplebranchstar

%type <Ast.param * Ast.expr> from
%type <(Ast.param * Ast.expr) list> fromstar

%type <Ast.types> typ
%type <Ast.types> typestar
%%


/* Règles de grammaire */

file:
  | s = stmt*; EOF {s}

stmt:
  | s = stmt_init {s}
  | s = stmt_final {s}

stmt_init:
  (*Function declaration*)
  | FUN; id = IDENT; x = funbody {Sconst (id, [], Taundef, Elam x)}
  | FUN; id = IDENT; LEFT_CHEV ; l = polymorph_args ; RIGHT_CHEV; x = funbody {Sconst (id, l, Taundef, Elam x)}
  (*Variable declaration*)
  | VAR; id = IDENT; DBLCOLON; t = typ; EQUAL; e = bexpr {Svar (id, Ta t, e)}
  | VAR; id = IDENT; EQUAL; e = bexpr {Svar (id, Taundef, e)}
  | id = IDENT;  DBLCOLON; t = typ;  EQUAL; e = bexpr {Sconst (id, [], Ta t, e)}
  | id = IDENT; EQUAL; e = bexpr {Sconst (id, [], Taundef, e)}

polymorph_args:
  | id = IDENT ; COMMA ; tl = polymorph_args {id :: tl}
  | id = IDENT {[id]}

stmt_final:
  | id = IDENT; DEF; e = bexpr {Saffect (id, e)}
  | b = bexpr {Sexpr b}


expr:
  | b = CBOOL {Ecst (Cbool b)}
  | i = CINT {Ecst (Cint i)}
  | s = CSTR {Ecst (Cstr s)}
  | id = IDENT {Evar id}
  | LP; b = bexpr; RP {b}
  | BLOCK; COLON; b = block; END {Eblock b}
  | LAM; f = funbody {Elam f}
  | CASES; LP; t = typ; RP; m = bexpr; branches = branches {Ecases (t,m,branches)}
  | c = caller; LP; b = bexprstar {Ecall (c,b)}
  | FOR; c = caller; LP;  p = fromstar; ARROW; t = typ; b = ublock; END {
    let parameters,expressions = List.split p in 
    Ecall (c, (Elam (Funbody (parameters, t, b)))::expressions)
  }
  | i = ifblock {i}

bexpr:
  | e = expr {e}
  | e1 = expr; b = BINOP; e2 = bexpr {Bexpr (b, e1, e2)}

bexprstar:
  | RP {[]}
  | b = bexpr; RP {[b]}
  | b = bexpr; COMMA; bs = bexprstar {b::bs}

identstar:
  | RP {[]}
  | id = IDENT; RP {[id]}
  | id = IDENT; COMMA; ids = identstar {id::ids}


block:
  | s1 = stmt*; s2 = stmt_final {s1@[s2]}

simpleblock:
  | s = stmt_final {[s]}
  | s1 = stmt_init; s2 = simpleblock {s1::s2}

ublock:
  | COLON ; b = simpleblock; {b}
  | BLOCK; COLON ; b = block {b}


funbody:
  | LP; p = funpar; ARROW; t = typ; b = ublock; END {Funbody (p,t,b)}

funpar:
  | RP {[]}
  | p = param; COMMA; f = funpar {p::f}
  | p = param; RP {[p]}

param:
  | id = IDENT; DBLCOLON; t = typ {(id, Ta t)}

caller:
  | c = IDENT; b = nonempty_list(call_arg)
  {
    List.fold_left (fun cal bexp -> Cfun (cal,bexp)) (Cvar c) b
  }

call_arg:
  | LP; b = bexprstar {b}


ifblock:
  | IF; e = bexpr; COLON; bif = simpleblock; ELSE; COLON; belse = simpleblock; END {Eif (e,Eblock bif,Eblock belse)}
  | IF; e = bexpr; BLOCK; COLON; bif = block; ELSE; COLON; belse = block; END {Eif (e,Eblock bif,Eblock belse)}
  | IF; e = bexpr; COLON; bif = simpleblock; ELSE; belse = simpleelif {Eif (e, Eblock bif, belse)}
  | IF; e = bexpr; BLOCK; COLON; bif = block; ELSE; belse = elif {Eif (e, Eblock bif, belse)}

elif:
  | IF; e = bexpr; COLON; bif = block; ELSE; belse = elif {Eif (e,Eblock bif,belse)}
  | IF; e = bexpr; COLON; bif = block; ELSE; COLON; belse = block; END {Eif (e, Eblock bif, Eblock belse)}
  | IF; e = bexpr; COLON; bif = block; END {Eif (e, Eblock bif, Ecall (Cvar "raise", [Ecst (Cstr "undefined else case")]))}

simpleelif:
  | IF; e = bexpr; COLON; bif = simpleblock; ELSE; belse = simpleelif {Eif (e,Eblock bif,belse)}
  | IF; e = bexpr; COLON; bif = simpleblock; ELSE; COLON; belse = simpleblock; END {Eif (e, Eblock bif, Eblock belse)}
  | IF; e = bexpr; COLON; bif = simpleblock; END {Eif (e, Eblock bif, Ecall (Cvar "raise", [Ecst (Cstr "undefined else case")]))}


branches:
  | COLON; b = simplebranchstar {b}
  | BLOCK; COLON; b = branchstar {b}

branch:
  | PIPE; id = IDENT; DOUBLEARROW; b = block {Branch1 (id,b)}
  | PIPE; id = IDENT; LP; idlist = identstar; DOUBLEARROW; b = block {Branch2 (id, idlist, b)}

simplebranch:
  | PIPE; id = IDENT; DOUBLEARROW; b = simpleblock {Branch1 (id,b)}
  | PIPE; id = IDENT; LP; idlist = identstar; DOUBLEARROW; b = simpleblock {Branch2 (id, idlist, b)}

branchstar:
  | END {[]}
  | b = branch; blist = branchstar {b::blist}

simplebranchstar:
  | END {[]}
  | b = simplebranch; blist = simplebranchstar {b::blist}


from:
  p = param; FROM; b = bexpr {(p, b)}

fromstar:
  | RP {[]}
  | f = from; RP {[f]}
  | f = from; COMMA; fs = fromstar {f::fs}


typ:
  | id = IDENT {Tcustom id}
  | LP; t1 = typestar; t2 = typ; RP {Tfun (t1, t2)}

typestar:
  | t = typ; ARROW {t}
  | t = typ; COMMA; ts = typestar {Tproduct (t, ts)}

