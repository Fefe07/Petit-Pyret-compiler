type binop =
  | Badd
  | Bsub
  | Bmul
  | Bdiv
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
  | Tvar of tvar    (* Une variable de type *)
  | Talpha of ident (* Un type polymorphe et son nom *)
  | Tany
  | Tboolean
  | Tint
  | Tstr
  | Tlist of types
  | Tnothing
  | Tproduct of types * types
  | Tfun of types list * types

and tvar = { id:int; mutable def : types option }

type type_annotation =
  | Taundef
  | Ta of string
  | Talist of string * type_annotation list
  | Tafun of type_annotation list * type_annotation

type cst = Cbool of bool | Cint of int | Cstr of string
type param = ident * type_annotation

type stmt =
  | Sexpr of expr
  | Saffect of ident * expr
  | Svar of ident * type_annotation * expr
  | Sconst of ident * type_annotation * expr 
  | Sfun of ident * ident list * param list * type_annotation * block

and block = stmt list

and expr =
  | Bexpr of binop * expr * expr
  | Ecst of cst
  | Evar of ident
  | Eblock of block
  | Elam of funbody
  | Ecall of expr * expr list
  | Ecases of type_annotation * expr * branch list
  | Eif of expr * expr * expr

and funbody = Funbody of param list * type_annotation * block
and branch = Branch1 of ident * block | Branch2 of ident * ident list * block
