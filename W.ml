Open Ast 
Open Parser


type typ =
  | Tint
  | Tarrow of typ * typ
  | Tproduct of typ * typ
  | Tvar of tvar

and tvar =
  { id : int;
    mutable def : typ option }

let rec pp_typ fmt = function
| Tproduct (t1, t2) -> Format.fprintf fmt "%a *@ %a" pp_atom t1 pp_atom t2
| Tarrow (t1, t2) -> Format.fprintf fmt "%a ->@ %a" pp_atom t1 pp_typ t2
| (Tint | Tvar _) as t -> pp_atom fmt t
and pp_atom fmt = function
| Tint -> Format.fprintf fmt "int"
| Tvar v -> pp_tvar fmt v
| Tarrow _ | Tproduct _ as t -> Format.fprintf fmt "@[<1>(%a)@]" pp_typ t
and pp_tvar fmt = function
| { def = None; id } -> Format.fprintf fmt "'%d" id
| { def = Some t; id } -> Format.fprintf fmt "@[<1>('%d := %a)@]" id pp_typ t


module V = struct
  type t = tvar
  let compare v1 v2 = Stdlib.compare v1.id v2.id
  let equal v1 v2 = v1.id = v2.id
  let create = let r = ref 0 in fun () -> incr r; { id = !r; def = None }
end


let rec head (t:typ): typ = 
  match t with
  | Tvar tv -> begin
    match tv.def with 
    | None -> t 
    | Some t' -> head t'
  end 
  | _ -> t 

let rec canon t = 
  let t' = head t in 
  match t' with 
  | Tint -> t' 
  | Tarrow (x,y) -> Tarrow(canon x, canon y)
  | Tproduct (x,y) -> Tproduct(canon x, canon y)
  | Tvar _ -> t'

let () =
  let a = V.create () in
  let b = V.create () in
  let ta = Tvar a in
  let tb = Tvar b in
  assert (head ta == ta);
  assert (head tb == tb);
  let ty = Tarrow (ta, tb) in
  a.def <- Some tb;
  assert (head ta == tb);
  assert (head tb == tb);
  b.def <- Some Tint;
  assert (head ta = Tint);
  assert (head tb = Tint);
  assert (canon ta = Tint);
  assert (canon tb = Tint);
  assert (canon ty = Tarrow (Tint, Tint))

exception UnificationFailure of typ * typ

let unification_error t1 t2 = raise (UnificationFailure (canon t1, canon t2))


let rec occur (tv : tvar)(t : typ) : bool = 
  (* Pourquoi head ? par ce que c'est récursif*)
  match head t with 
  | Tvar tv' when tv.id = tv'.id -> true 
  | Tvar tv' -> begin match tv'.def with Some t'' -> occur tv t'' | None -> false end
  | Tarrow(x,y) | Tproduct(x,y) -> occur tv x || occur tv y 
  | Tint -> false 



let rec unify (t : typ)(t':typ): unit = 
  match head t, head t' with 
  | Tarrow(x1,y1),Tarrow(x2,y2) | Tproduct(x1,y1),Tproduct(x2,y2) -> unify x1 x2 ; unify y1 y2 
  | Tint, Tint -> () 
  | Tvar tv1, Tvar tv2 -> unify (Option.get tv1.def) (Option.get tv2.def) 
  | Tvar tv, x | x, Tvar tv -> tv.def<-Some x
  | _ -> unification_error t t'

let () =
  let a = V.create () in
  let b = V.create () in
  let ta = Tvar a in
  let tb = Tvar b in
  assert (occur a ta);
  assert (occur b tb);
  assert (not (occur a tb));
  let ty = Tarrow (ta, tb) in
  assert (occur a ty);
  assert (occur b ty);
  (* unifie 'a-> 'b et int->int *)
  unify ty (Tarrow (Tint, Tint));
  assert (canon ta = Tint);
  assert (canon ty = Tarrow (Tint, Tint));
  (* unifie 'c et int->int *)
  let c = V.create () in
  let tc = Tvar c in
  unify tc ty;
  assert (canon tc = Tarrow (Tint, Tint))

let cant_unify ty1 ty2 =
  try let _ = unify ty1 ty2 in false with UnificationFailure _ -> true

let () =
  assert (cant_unify Tint (Tarrow (Tint, Tint)));
  assert (cant_unify Tint (Tproduct (Tint, Tint)));
  let a = V.create () in
  let ta = Tvar a in
  unify ta (Tarrow (Tint, Tint));
  assert (cant_unify ta Tint)


module Vset = Set.Make(V)

let rec fvars (t : typ) : Vset.t = 
  match head t with 
  | Tint -> Vset.empty
  | Tarrow (x,y) | Tproduct(x,y) -> Vset.union (fvars x) (fvars y)
  | Tvar tv -> Vset.singleton tv
  

let () =
  assert (Vset.is_empty (fvars (Tarrow (Tint, Tint))));
  let a = V.create () in
  let ta = Tvar a in
  let ty = Tarrow (ta, ta) in
  assert (Vset.equal (fvars ty) (Vset.singleton a));
  unify ty (Tarrow (Tint, Tint));
  assert (Vset.is_empty (fvars ty))

type schema = { vars : Vset.t; typ : typ }

module Smap = Map.Make(String)

type env = { bindings : schema Smap.t; fvars : Vset.t }

let empty = {bindings = Smap.empty ; fvars = Vset.empty}

(* val add : string -> typ -> env -> env *)

let add (s : string)(t : typ) (e : env) : env = 
  let schem = {vars = Vset.empty ; typ = t} in 
  let bindings' = Smap.add s schem e.bindings in 
  let fvars' = Vset.union e.fvars (fvars t) in 
  {bindings = bindings'; fvars = fvars'}

let add_gen (s : string)(t : typ)(e : env) : env = 
  let free_vars = Vset.diff (fvars t) (e.fvars) in 
  let schem = {vars = free_vars ; typ = t} in  
  let bindings' = Smap.add s schem e.bindings in 
  {bindings = bindings'; fvars = e.fvars}


let find (s : string)(e : env) : typ = 
  let schem = Smap.find s e.bindings in 
  let new_variables = ref [] in 
  let rec explore_type t' : typ =
    match head t' with 
    | Tint -> Tint 
    | Tarrow(x,y) -> Tarrow(explore_type x, explore_type y)
    | Tproduct(x,y) -> Tproduct(explore_type x, explore_type y)
    | Tvar tv -> 
      try Tvar (List.assoc tv !new_variables)
      with Not_found -> let new_tv = V.create () in new_variables := (tv, new_tv) :: !new_variables ; Tvar new_tv
  in 
  explore_type schem.typ


type expression =
  | Var of string
  | Const of int
  | Op of string
  | Fun of string * expression
  | App of expression * expression
  | Pair of expression * expression
  | Let of string * expression * expression


let rec w (e : env)(expr : expression) : typ = 
  match expr with
  | Var s -> find s e
  | Const n -> Tint 
  | Op x -> find x e 
  | Pair(x,y) -> Tproduct(w e x, w e y)
  | Fun(x,exp) -> let new_tv = V.create () in  Tarrow(Tvar new_tv, w (add x (Tvar new_tv) e) exp)
  | App(f,x) -> 
    let t1 = w e f in 
    let t2 = w e x in 
    let t' = Tvar (V.create()) in
    unify t1 (Tarrow(t2,t')) ;
    t'
  | Let(x, e1, e2) -> 
    let t1 = w e e1 in 
    let t2 = w (add_gen x t1 e) e2 in t2

let typeof e = canon (w empty e)

(* 1 : int *)
let () = assert (typeof (Const 1) = Tint)

(* fun x -> x : 'a -> 'a *)
let () = assert (match typeof (Fun ("x", Var "x")) with
  | Tarrow (Tvar v1, Tvar v2) -> V.equal v1 v2
  | _ -> false)

(* fun x -> x+1 : int -> int *)
let () = assert (typeof (Fun ("x", App (Op "+", Pair (Var "x", Const 1))))
                 = Tarrow (Tint, Tint))

(* fun x -> x+x : int -> int *)
let () = assert (typeof (Fun ("x", App (Op "+", Pair (Var "x", Var "x"))))
                 = Tarrow (Tint, Tint))

(* let x = 1 in x+x : int *)
let () =
  assert (typeof (Let ("x", Const 1, App (Op "+", Pair (Var "x", Var "x"))))
          = Tint)

(* let id = fun x -> x in id 1 *)
let () =
  assert (typeof (Let ("id", Fun ("x", Var "x"), App (Var "id", Const 1)))
          = Tint)

(* let id = fun x -> x in id id 1 *)
let () =
  assert (typeof (Let ("id", Fun ("x", Var "x"),
		       App (App (Var "id", Var "id"), Const 1)))
          = Tint)

(* let id = fun x -> x in (id 1, id (1,2)) : int * (int * int) *)
let () =
  assert (typeof (Let ("id", Fun ("x", Var "x"),
		       Pair (App (Var "id", Const 1),
			     App (Var "id", Pair (Const 1, Const 2)))))
          = Tproduct (Tint, Tproduct (Tint, Tint)))

(* app = fun f x -> let y = f x in y : ('a -> 'b) -> 'a -> 'b *)
let () =
  let ty =
    typeof (Fun ("f", Fun ("x", Let ("y", App (Var "f", Var "x"), Var "y"))))
  in
  assert (match ty with
    | Tarrow (Tarrow (Tvar v1, Tvar v2), Tarrow (Tvar v3, Tvar v4)) ->
        V.equal v1 v3 && V.equal v2 v4
    | _ -> false)