type binop =
  | Badd
  | Baddstr
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

module Smap = Map.Make(String)
module Fmap = Map.Make(Int)

type local_env = int Smap.t


type types =
  | Tvar of tvar    (* Une variable de type *)
  | Talpha of ident (* Un type polymorphe et son nom *)
  | Tany
  | Tboolean
  | Tint
  | Tstr
  | Tlist of types
  | Tnothing
  | Tfun of types list * types
  | Tintstr

and tvar = { id:int; mutable def : types option }

type type_annotation =
  | Taundef
  | Ta of string
  | Talist of string * type_annotation list
  | Tafun of type_annotation list * type_annotation

type cst = Cbool of bool | Cint of int | Cstr of string
type param = ident * type_annotation

type frame_size = int

type stmt =
  | Sexpr of expr
  | Saffect of ident * expr
  | Svar of ident * type_annotation * expr
  | Sconst of ident * type_annotation * expr 
  | Sfun of ident * ident list * param list * type_annotation * block
  | Sfun2 of ident * ident list * block

and astmt = 
  | Aexpr of aexpr * frame_size
  | Aaffect of int * aexpr * frame_size
  | Avar of ident * aexpr * frame_size
  | Aconst of ident * aexpr * frame_size 
  | Afun of ident * ident list * ablock * frame_size * fermeture * int

and block = stmt list
and ablock = astmt list

and expr =
  | Bexpr of binop * expr * expr
  | Ecst of cst
  | Evar of ident
  | Eblock of block
  | Elam of funbody
  | Ecall of expr * expr list
  | Ecases of type_annotation * expr * branch list
  | Eif of expr * expr * expr

and aexpr = 
  | Abexpr of binop * aexpr * aexpr
  | Acst of cst
  | Aident of int
  | Aferm of int
  | Ablock of ablock
  | Alam of afunbody
  | Acall of aexpr * aexpr list
  | Acases of type_annotation * aexpr * abranch list
  | Aif of aexpr * aexpr * aexpr
  | Aprint of aexpr

and funbody = Funbody of param list * type_annotation * block
and afunbody = Afunbody of param list * type_annotation * ablock
and branch = Branch1 of ident * block | Branch2 of ident * ident list * block
and abranch = Branch1 of ident * ablock | Branch2 of ident * ident list * ablock
and fermeture = aexpr Fmap.t


let rec print_expr = function 
  | Eif(e1,e2,e3) -> 
    (print_string "Eif\nCondition :\n" ; 
    print_expr e1 ;
    print_string "Si :\n" ; 
    print_expr e2 ;
    print_string "Sinon :\n" ; 
    print_expr e3)
  | Bexpr (_,e1,e2) -> (print_string "Binop\n"; print_expr e1; print_expr e2)
  | Ecall (e, l) -> print_string "Appel" ; print_expr e ; List.iter print_expr l 
  | Ecst c -> begin 
    match c with 
    | Cstr s -> print_string s 
    | Cint i -> print_int i 
    | Cbool b -> if b then print_string "true" else print_string "false"
  end
  | Eblock b -> print_string "block:" ; List.iter print_stmt b
  | Evar v -> print_string ("var"^v)
  | _ -> print_string "Autre\n"

and print_stmt = function
| Sexpr e -> print_expr e 
| _ -> print_string "Autre stmt\n"
