open Ast 
open Format
exception UnificationFailure of types * types
exception RedefinedVariable of string
exception NotCallable of types
exception UnknownAnnotation of type_annotation
exception WrongCase
exception WrongArgsNumber of int * int
exception VariableNotFound of string

module V = struct 
  type t = tvar
  let compare v1 v2 = Stdlib.compare v1.id v2.id 
  let equal v1 v2 = v1.id = v2.id 
  let create = let r = ref 0 in fun () -> 
    incr r;
    { id = !r; def = None }
end

let rec head (t:types): types =
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
  | Tvar _ | Tint | Tstr | Tboolean 
  | Tany | Tnothing | Talpha _ -> t' 
  | Tfun (x,y) -> Tfun(List.map canon x, canon y)
  | Tproduct (x,y) -> Tproduct(canon x, canon y)
  | Tlist x -> Tlist (canon x)

let rec occur (tv : tvar)(t : types) : bool = 
  match head t with 
  | Tvar tv' when tv.id = tv'.id -> true 
  | Tvar tv' -> begin match tv'.def with
      | Some t'' -> occur tv t''
      | None -> false end
  | Tfun(x,y) -> List.exists (occur tv) x || occur tv y
  | Tproduct(x,y) -> occur tv x || occur tv y 
  | Tint | Tstr | Tany | Tnothing | Tboolean | Talpha _ -> false
  | Tlist x -> occur tv x

let unification_error t1 t2 =
  raise (UnificationFailure (canon t1, canon t2))

let rec unify (t : types) (t':types): unit = 
  match head t, head t' with 
  | Tfun (x1,y1), Tfun (x2,y2) -> begin
      try
        List.iter2 unify x1 x2;
        unify y1 y2
      with | Invalid_argument _ -> raise (UnificationFailure (t,t'))
    end
  | Tproduct(x1,y1),Tproduct(x2,y2) -> unify x1 x2 ; unify y1 y2

  | Tint, Tint | Tstr, Tstr | Tany, Tany | Tnothing, Tnothing 
  | Tboolean, Tboolean -> ()

  | Tlist x, Tlist y -> unify x y
  | Tvar tv, Tvar tv' -> begin
      match tv.def, tv'.def with
      | None, _ -> tv.def <- Some (Tvar tv')
      | _, None -> tv'.def <- Some (Tvar tv)
      | Some t1, Some t2 -> unify t1 t2 end
  | Tvar tv, x | x, Tvar tv -> begin
      match tv.def with
      | None -> tv.def <- Some x
      | Some t1 -> unify t1 x end
  | Talpha x, Talpha y when x = y -> ()
  | _ -> unification_error t t'

module Vset = Set.Make(V)

let rec fvars (t : types) : Vset.t = 
  match head t with 
  | Tint | Tstr | Tboolean | Talpha _ | Tany | Tnothing -> Vset.empty
  | Tfun (x,y) ->
      List.fold_left (fun a b -> Vset.union a (fvars b)) (fvars y) x
  | Tproduct(x,y) -> Vset.union (fvars x) (fvars y)
  | Tlist x -> fvars x
  | Tvar tv -> Vset.singleton tv

module Smap = Map.Make(String)
module Sset = Set.Make(String)
type schema = { vars : Sset.t; typ : types }

type env = {
  bindings : schema Smap.t;
  var_bindings : types Smap.t;
  types : Sset.t;
  fvars : Vset.t }

let rec bien_forme environment typ = 
  match head typ with
  | Tint | Tstr | Tboolean | Tany | Tnothing -> true
  | Tfun (x,y) -> List.for_all (bien_forme environment) x
    && bien_forme environment y
  | Tproduct (x,y) ->
      bien_forme environment x && bien_forme environment y
  | Tlist x -> bien_forme environment x
  | Talpha x -> Sset.mem x environment.types
  | Tvar _ -> false (* Je ne sais pas quoi faire ici *)

let start_environment = {
  bindings = Smap.of_list [
    "nothing", { vars = Sset.empty; typ = Tnothing };
    "num-modulo", {
      vars = Sset.empty; typ = Tfun ([Tint; Tint], Tint)
    };
    "empty", { vars = Sset.singleton "A"; typ = Tlist (Talpha "A") };
    "link", {
      vars = Sset.singleton "A";
      typ = Tfun ([Talpha "A"; Tlist (Talpha "A")],
          Tlist (Talpha "A"))
    };
    "print", {
      vars = Sset.singleton "A";
      typ = Tfun ([Talpha "A"], Talpha "A")
    };
    "raise", {
      vars = Sset.singleton "A";
      typ = Tfun ([Tstr], Talpha "A")
    };
    "each", {
      vars = Sset.of_list ["A";"B"];
      typ = Tfun ([Tfun ([Talpha "A"], Talpha "B"); Tlist (Talpha "A")], Tnothing)
    };
    "fold", {
      vars = Sset.of_list ["A"; "B"];
      typ = Tfun ([
        Tfun ([Talpha "A"; Talpha "B"], Talpha "A");
        Talpha "A";
        Tlist (Talpha "B")],
        Talpha "A")
    }
  ];
  var_bindings = Smap.empty;
  types = Sset.empty;
  fvars = Vset.empty;
}

let add_bind environment name typ =
  if Smap.mem name environment.bindings ||
      Smap.mem name environment.var_bindings
  then raise (RedefinedVariable name)
  else
    let scheme = {vars = Sset.empty; typ = typ} in
    let fvars = Vset.union environment.fvars (fvars typ) in
    {
      bindings = Smap.add name scheme environment.bindings;
      var_bindings = environment.var_bindings;
      types = environment.types;
      fvars = fvars
    }

let add_var environment name typ = 
  if Smap.mem name environment.bindings ||
      Smap.mem name environment.var_bindings
  then raise (RedefinedVariable name)
  else
    let fvars = Vset.union environment.fvars (fvars typ) in
    {
      bindings = environment.bindings;
      var_bindings = Smap.add name typ environment.var_bindings;
      types = environment.types;
      fvars = fvars
    }

let add_poly environment poly = 
  let def_types = Sset.of_list
      ["Number"; "Any"; "Boolean"; "Nothing"; "String";"List"] in
  let add_one env p =
    if Sset.mem p env.types || Sset.mem p def_types 
    then raise (RedefinedVariable p)
    else
      {
        bindings = environment.bindings;
        var_bindings = environment.var_bindings;
        types = Sset.add p environment.types;
        fvars = environment.fvars
    }
  in List.fold_left add_one environment poly

let add_schema environment name s =
  if Smap.mem name environment.bindings ||
      Smap.mem name environment.var_bindings
  then raise (RedefinedVariable name)
  else
    {
      bindings = Smap.add name s environment.bindings;
      var_bindings = environment.var_bindings;
      types = environment.types;
      fvars = environment.fvars
    }
  

let rec read_ta environment ta = 
  match ta with
  | Taundef -> Tvar (V.create ())
  | Talist (id, typs) -> begin
      if id <> "List" then raise (UnknownAnnotation ta)
      else
      match typs with
      | [x] -> Tlist (read_ta environment x)
      | _ -> raise (UnknownAnnotation ta) end
  | Tafun (talist, ta) ->
      Tfun (List.map (read_ta environment) talist,
      read_ta environment ta)
  | Ta id -> begin
      match id with
      | "Number" -> Tint
      | "Any" -> Tany
      | "Boolean" -> Tboolean
      | "Nothing" -> Tnothing
      | "String" -> Tstr
      | _ ->
          if Sset.mem id environment.types then Talpha id 
          else raise (UnknownAnnotation ta)
      end

let find e id = 
  if Smap.mem id e.var_bindings then Smap.find id e.var_bindings
  else try
    let s = Smap.find id e.bindings in
    let rec fresh_type quant maps t = 
      match head t with
      | Tany | Tnothing | Tboolean | Tstr | Tint -> head t
      | Tlist x -> Tlist (fresh_type quant maps x)
      | Tfun (t_list, tret) ->
          Tfun (List.map (fresh_type quant maps) t_list,
            fresh_type quant maps tret)
      | Tproduct (t1, t2) -> Tproduct (fresh_type quant maps t1,
          fresh_type quant maps t1)
      | Talpha a -> if Hashtbl.mem maps a then Hashtbl.find maps a
          else if Sset.mem a quant
            then let newmap = Tvar (V.create ()) in 
            (Hashtbl.add maps a newmap; newmap)
          else Talpha a
      | Tvar t -> Tvar t in
    fresh_type s.vars (Hashtbl.create 16) s.typ
  with | Not_found -> raise (VariableNotFound id)

let rec w environment stmts =
  let (_, ret_type) = List.fold_left
    (fun (e,t) s -> w_stmt e s) (environment, Tnothing) stmts 
  in ret_type 

and w_stmt environment stmt = 
  match stmt with 
  | Sexpr e -> (environment, w_expr e environment)
  | Saffect (id,e) -> (
    try
      unify (Smap.find id environment.var_bindings)
            (w_expr e environment);
      (environment, Smap.find id environment.var_bindings)
    with | Not_found -> raise (VariableNotFound id))
  | Svar (id, ta, e) ->
    let r_type = read_ta environment ta in begin
      unify r_type (w_expr e environment);
      (add_var environment id r_type, Tnothing) end
  | Sconst (id, poly, ta, e) ->
    let new_env = add_poly environment poly in
    let ret_type = read_ta new_env ta in 
    let t = (w_expr e new_env) in (
    unify ret_type t;
    add_schema environment id {vars = Sset.of_list poly; typ = ret_type},
    Tnothing)


and w_expr exp environment = 
  match exp with
  | Bexpr (op,e1,e2) ->  begin match op with 
    | Badd | Bsub | Bmul | Bdiv -> begin 
      unify (w_expr e1 environment) Tint;
      unify (w_expr e2 environment) Tint;
      Tint
    end
    | Beq | Bneq -> begin
      w_expr e1 environment;
      w_expr e2 environment;
      Tboolean
    end 
    | Band | Bor -> begin
      unify (w_expr e1 environment) Tboolean;
      unify (w_expr e2 environment) Tboolean;
      Tboolean
    end 
    | Ble | Blt | Bge | Bgt -> begin
      unify (w_expr e1 environment) Tint;
      unify (w_expr e2 environment) Tint;
      Tboolean
    end end
  | Ecst cst -> begin match cst with 
      | Cbool _ -> Tboolean
      | Cint  _ -> Tint
      | Cstr  _ -> Tstr
    end
  | Evar id -> find environment id
  | Eblock b -> w environment b
  | Eif (e1, e2, e3) -> begin
      unify (w_expr e1 environment) Tboolean;
      let ret_type = w_expr e2 environment in
      unify ret_type (w_expr e3 environment);
      ret_type
    end
  | Ecall (func, args) ->
      let f_type = w_expr func environment in begin
      match f_type with
      | Tfun (targs, tret) -> begin
        try
        (
          List.iter2 (fun x y -> unify x (w_expr y environment))
              targs args;
          tret
        )
        with |Invalid_argument _ -> raise
          (WrongArgsNumber ((List.length targs),(List.length args)))
        end
      | _ -> raise (NotCallable f_type) end
  | Ecases (ta, e, branches) ->
      let t = w_expr e environment in 
      let sub_type = Tvar (V.create ()) in begin
      unify t (Tlist sub_type);
      unify t (read_ta environment ta);
      match branches with
      | (Branch1 ("empty", be))::(Branch2 ("link", [x;y], bl))::[]
      | (Branch2 ("link", [x;y], bl))::(Branch1 ("empty", be))::[] ->
          let new_env = add_bind (add_bind environment x sub_type) y t in
          let t_ret = w new_env bl in
          begin unify t_ret (w environment be);
          t_ret end
      | _ -> raise WrongCase
      end

  | Elam (Funbody (params, ta, b)) -> 
      let new_env = List.fold_left
      (fun e (id,t_a) -> add_bind e id (read_ta environment t_a))
      environment params in
      let start_types = List.fold_right (
        fun (id, t_a) l -> (read_ta environment t_a)::l
      ) params [] in
      let r_type = w new_env b in 
      (unify r_type (read_ta new_env ta); Tfun(start_types, r_type))

let typing s = w start_environment s
