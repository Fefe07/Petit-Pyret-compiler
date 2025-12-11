type binop =
  | Badd
  | Bsub
  | Bmul
  | Bdiv (* + - * /  *)
  | Beq
  | Bneq
  | Blt
  | Ble
  | Bgt
  | Bge (* == <> < <= > >= *)
  | Band
  | Bor (* && || *)

type ident = string

type types =
  (*
  | Talpha
  | Tany
  | Tnothing
  | Tboolean
  | Tint 
  | Tstr
  | Tlist of types *)
  | Tproduct of types * types
  | Tfun of types * types
  | Tcustom of string

type type_annotation = Taundef | Ta of types
type cst = Cbool of bool | Cint of int | Cstr of string
type param = ident * type_annotation

type stmt =
  | Sexpr of expr
  | Saffect of ident * expr
  | Svar of ident * type_annotation * expr
  | Sconst of ident * string list * type_annotation * expr (*Types génériques ajoutés*)

and block = stmt list

and expr =
  | Bexpr of binop * expr * expr
  | Ecst of cst
  | Evar of ident
  | Eblock of block
  | Elam of funbody
  | Ecall of caller * expr list
  | Ecases of types * expr * branch list
  | Eif of expr * expr * expr

and funbody = Funbody of param list * types * block
and caller = Cfun of caller * expr list | Cvar of ident
and branch = Branch1 of ident * block | Branch2 of ident * ident list * block
