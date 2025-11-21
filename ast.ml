type binop =
 | Badd | Bsub | Bmul | Bdiv           (* + - * /  *)
 | Beq | Bneq | Blt | Ble | Bgt | Bge  (* == <> < <= > >= *)
 | Band | Bor                          (* && || *)


type ident = string

type types = (*
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

type type_annotation = 
  | Taundef
  | Ta of types

type cst = 
  | Cbool of bool
  | Cint of int
  | Cstr of string

type param = ident * type_annotation

type stmt =
  | Sexpr of expr
  | Saffect of ident * expr
  | Sfun of ident * param list * type_annotation * block
  | Svar of ident * type_annotation * expr


and block = stmt list

and expr = 
  | Bexpr of binop * expr * expr
  | Ecst of cst
  | Evar of ident
  | Eblock of block

type funbody = Funbody of param list * type_annotation * block


