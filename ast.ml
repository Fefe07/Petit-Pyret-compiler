type binop =
 | Badd | Bsub | Bmul | Bdiv           (* + - * /  *)
 | Beq | Bneq | Blt | Ble | Bgt | Bge  (* == <> < <= > >= *)
 | Band | Bor                          (* && || *)


type ident = string

type types = 
  | Talpha
  | Tany
  | Tnothing
  | Tboolean
  | Tint 
  | Tstr 
  | Tlist of types 
  | Tfun of types * types

type type_annotation = 
  | Taundef
  | Ta of types

type expr = 
  | Bexpr of binop * expr * expr

type stmt =
  | Sexpr of expr

type Var of ident * type_annotation * expr

type block = stmt list
