  /* Analyseur syntaxique pour petit-Pyret */

%{
  open Ast

%}

/* Déclaration des tokens */

%token LEFT_CHEV RIGHT_CHEV EOF BLOCK CASES ELSE END FOR FROM FUN IF LAM VAR 
%token EQUAL LP RP LP_CALL COMMA COLON DBLCOLON ARROW DOUBLEARROW PIPE 
%token PLUS MINUS MUL DIV DEF LT GT
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
%type <Ast.expr> plusexpr
%type <Ast.expr> minexpr
%type <Ast.expr> divexpr
%type <Ast.expr> mulexpr

%type <Ast.block> block
%type <Ast.block> simpleblock
%type <Ast.block> ublock

%type <Ast.funbody> funbody
%type <Ast.param list> funpar
%type <Ast.param> param
%type <Ast.expr> caller
%type <Ast.expr list list> call_argstar
%type <Ast.ident list> polymorph_args

%type <Ast.expr> ifblock
%type <Ast.expr> elif
%type <Ast.expr> simpleelif

%type <Ast.branch list> branches
%type <Ast.branch> branch
%type <Ast.branch> simplebranch
%type <Ast.branch list> branchstar
%type <Ast.branch list> simplebranchstar

%type <Ast.expr * ((Ast.param * Ast.expr) list)> forcaller
%type <(Ast.expr list list)*((Ast.param * Ast.expr) list)> arguments
%type <Ast.param * Ast.expr> from
%type <(Ast.param * Ast.expr) list> fromstar

%type <Ast.type_annotation> typ
%type <Ast.type_annotation list> typestar
%type <Ast.type_annotation list> typ_params
%%


/* Règles de grammaire */

file:
  | s = stmt*; EOF {s}

stmt:
  | s = stmt_init {s}
  | s = stmt_final {s}

stmt_init:
  (*Function declaration*)
  | FUN; id = IDENT; x = funbody
    {let Funbody (params, ret, b) = x in 
    Sfun (id, [], params, ret, b)}
  | FUN; id = IDENT; LEFT_CHEV ; l = polymorph_args ; RIGHT_CHEV; x = funbody 
  {let Funbody (params, ret, b) = x in 
    Sfun (id, l, params, ret, b)}
  (*Deguelasse mais au moins ça marche *)
  | FUN; id = IDENT; LT ; l = polymorph_args ; RIGHT_CHEV; x = funbody 
  {let Funbody (params, ret, b) = x in 
      Sfun (id, l, params, ret, b)}
  (*{(if b <> Blt then 
  failwith "Erreur syntaxique : opérateur binaire autre que < 
    après un nom de fonction");
  Sconst (id, l, Taundef, Elam x)}*)
  (*Variable declaration*)
  | VAR; id = IDENT; DBLCOLON; t = typ; EQUAL; e = bexpr {Svar (id, t, e)}
  | VAR; id = IDENT; EQUAL; e = bexpr {Svar (id, Taundef, e)}
  | id = IDENT;  DBLCOLON; t = typ;  EQUAL; e = bexpr {Sconst (id, t, e)}
  | id = IDENT; EQUAL; e = bexpr {Sconst (id, Taundef, e)}

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
  | CASES; LP; t = typ; RP; m = bexpr; branches = branches 
  {Ecases (t,m,branches)}
  | c = caller {c}
  | FOR; x = forcaller; ARROW; t = typ; b = ublock; END {
    let c,p = x in
    let parameters,expressions = List.split p in 
    Ecall (c, (Elam (Funbody (parameters, t, b)))::expressions)
  }
  | i = ifblock {i}

bexpr:
  | e = expr {e}
  | e1 = expr; b = BINOP; e2 = bexpr {Bexpr (b, e1, e2)}
  | e1 = expr; b = LT; e2 = plusexpr {Bexpr (Blt, e1, e2)}
  | e1 = expr; b = GT; e2 = plusexpr {Bexpr (Bgt, e1, e2)}
  | e1 = expr; b = PLUS; e2 = plusexpr {Bexpr (Badd, e1, e2)}
  | e1 = expr; b = MINUS; e2 = minexpr {Bexpr (Bsub, e1, e2)}
  | e1 = expr; b = MUL; e2 = mulexpr {Bexpr (Bmul, e1, e2)}
  | e1 = expr; b = DIV; e2 = divexpr {Bexpr (Bdiv, e1, e2)}

bexprstar:
  | b = bexpr; RP {[b]}
  | b = bexpr; COMMA; bs = bexprstar {b::bs}

identstar:
(*Not needed : Constructor with 0 arguments are noted without () *)
(*  | RP {[]}*) 
  | id = IDENT; RP {[id]}
  | id = IDENT; COMMA; ids = identstar {id::ids}

plusexpr:
  | e = expr {e}
  | e1 = expr; b = PLUS; e2 = plusexpr {Bexpr (Badd, e1, e2)}

minexpr:
  | e = expr {e}
  | e1 = expr; b = MINUS; e2 = minexpr {Bexpr (Bsub, e1, e2)}

mulexpr:
  | e = expr {e}
  | e1 = expr; b = MUL; e2 = mulexpr {Bexpr (Bmul, e1, e2)}

divexpr:
  | e = expr {e}
  | e1 = expr; b = DIV; e2 = divexpr {Bexpr (Bdiv, e1, e2)}


block:
  | s = stmt_final {[s]}
  | s = stmt; b = block {s::b}

simpleblock:
  | s = stmt_final {[s]}
  | s1 = stmt_init; s2 = simpleblock {s1::s2}

ublock:
  | COLON ; b = simpleblock; {b}
  | BLOCK; COLON ; b = block {b}


funbody:
  | LP_CALL; p = funpar; ARROW; t = typ; b = ublock; END {Funbody (p,t,b)}

funpar:
  | RP {[]}
  | p = param; COMMA; f = funpar {p::f}
  | p = param; RP {[p]}

param:
  | id = IDENT; DBLCOLON; t = typ {(id, t)}

caller:
  | c = IDENT; b = call_argstar
  {
    List.fold_left (fun cal bexp -> Ecall (cal,bexp)) (Evar c) b
  }

call_argstar:
  | LP_CALL ; RP {[]}
  | LP_CALL; b = bexprstar {[b]}
  | LP_CALL; b = bexprstar; b2 = call_argstar {b::b2}

polymorph_args:
  | id = IDENT ; COMMA ; tl = polymorph_args {id :: tl}
  | id = IDENT {[id]}


ifblock:
  | IF; e = bexpr; COLON; bif = simpleblock; 
    ELSE; COLON; belse = simpleblock; END
  {Eif (e,Eblock bif,Eblock belse)}
  | IF; e = bexpr; BLOCK; COLON; bif = block; ELSE; COLON; belse = block; END
  {Eif (e,Eblock bif,Eblock belse)}
  | IF; e = bexpr; COLON; bif = simpleblock; ELSE; belse = simpleelif
  {Eif (e, Eblock bif, belse)}
  | IF; e = bexpr; BLOCK; COLON; bif = block; ELSE; belse = elif
  {Eif (e, Eblock bif, belse)}

elif:
  | IF; e = bexpr; COLON; bif = block; ELSE; belse = elif
  {Eif (e,Eblock bif,belse)}
  | IF; e = bexpr; COLON; bif = block; ELSE; COLON; belse = block; END
  {Eif (e, Eblock bif, Eblock belse)}
  | IF; e = bexpr; COLON; bif = block; END
  {Eif (e, Eblock bif, 
    Ecall (Evar "raise", [Ecst (Cstr "undefined else case")])
  )}

simpleelif:
  | IF; e = bexpr; COLON; bif = simpleblock; ELSE; belse = simpleelif
  {Eif (e,Eblock bif,belse)}
  | IF; e = bexpr; COLON; bif = simpleblock; ELSE; COLON; belse = simpleblock; END
  {Eif (e, Eblock bif, Eblock belse)}
  | IF; e = bexpr; COLON; bif = simpleblock; END
  {Eif (e, Eblock bif, 
    Ecall (Evar "raise", [Ecst (Cstr "undefined else case")])
  )}


branches:
  | COLON; b = simplebranchstar {b}
  | BLOCK; COLON; b = branchstar {b}

branch:
  | PIPE; id = IDENT; DOUBLEARROW; b = block {Branch1 (id,b)}
  | PIPE; id = IDENT; LP_CALL; idlist = identstar; DOUBLEARROW; b = block
  {Branch2 (id, idlist, b)}

simplebranch:
  | PIPE; id = IDENT; DOUBLEARROW; b = simpleblock {Branch1 (id,b)}
  | PIPE; id = IDENT; LP_CALL; idlist = identstar; DOUBLEARROW; b = simpleblock
  {Branch2 (id, idlist, b)}

branchstar:
  | END {[]}
  | b = branch; blist = branchstar {b::blist}

simplebranchstar:
  | END {[]}
  | b = simplebranch; blist = simplebranchstar {b::blist}


forcaller:
  | id = IDENT; a = arguments {
    let l,f = a in 
    let call = List.fold_left (fun cal bexp -> Ecall (cal,bexp)) (Evar id) l
    in (call,f)
    
  }

arguments:
  | LP_CALL; f = fromstar {([],f)}
  | LP_CALL; b = bexprstar; a = arguments {let l,f = a in (b::l,f)}

from:
  p = param; FROM; b = bexpr {(p, b)}

fromstar:
  | RP {[]}
  | f = from; RP {[f]}
  | f = from; COMMA; fs = fromstar {f::fs}


typ:
  | id = IDENT {Ta id}
  | id = IDENT; LEFT_CHEV; t = typ_params {Talist (id,t)}
  (* Déguelasse mais fonctionnel. Lever d'erreur à améliorer *)
  | id = IDENT; LT; t = typ_params {(*assert(b==Blt);*)  Talist (id,t)}
  | LP; t1 = typestar; t2 = typ RP {Tafun (t1, t2)}

typestar:
  | ARROW {[]} 
  | t = typ; ARROW {[t]}
  | t = typ; COMMA; ts = typestar {t::ts}

typ_params:
  | t = typ; RIGHT_CHEV {[t]}
  (* Toujours aussi moche *)
  | t = typ; GT {(*assert(b==Bgt) ;*) [t]}
  | t = typ; COMMA; t2 = typ_params {t::t2}
