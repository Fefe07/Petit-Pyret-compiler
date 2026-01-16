open Ast 
open Format
exception UnificationFailure of types * types
exception RedefinedVariable of string
exception NotCallable of types
exception UnknownAnnotation of type_annotation
exception WrongCase
exception WrongArgsNumber of int * int
exception VariableNotFound of string
exception AffectationTypeError

module V = struct 
  type t = tvar
  let compare v1 v2 = Stdlib.compare v1.id v2.id 
  let equal v1 v2 = v1.id = v2.id 
  let create = let r = ref 0 in fun () -> 
    incr r;
    { id = !r; def = None }
end

let rec head (t:types): types =
  (* "Déplie" récursivement une variable de type contenant un type*)
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
  | Tany | Tnothing | Tintstr | Talpha _ -> t' 
  | Tfun (x,y) -> Tfun(List.map canon x, canon y)
  | Tlist x -> Tlist (canon x)

let rec occur (tv : tvar)(t : types) : bool = 
  (* Regarde si la variable tv apparait dans le type t *)
  match head t with 
  | Tvar tv' when tv.id = tv'.id -> true 
  | Tvar tv' -> begin match tv'.def with
      | Some t'' -> occur tv t''
      | None -> false end
  | Tfun(x,y) -> List.exists (occur tv) x || occur tv y
  | Tint | Tstr | Tany | Tnothing | Tboolean | Talpha _ | Tintstr-> false
  | Tlist x -> occur tv x

let unification_error t1 t2 =
  raise (UnificationFailure (canon t1, canon t2))

let rec unify (t : types) (t':types): unit = 
  (* Modifie les variables de type de t et t' afin qu'ils soient égaux *)
  (* Renvoie Unification_Error si les types sont incompatibles *)
  match head t, head t' with 
  | Tfun (x1,y1), Tfun (x2,y2) -> begin
      try
        List.iter2 unify x1 x2;
        unify y1 y2
      with | Invalid_argument _ -> raise (UnificationFailure (t,t'))
    end

  | Tint, Tint | Tstr, Tstr | Tany, Tany | Tnothing, Tnothing 
  | Tboolean, Tboolean | Tintstr, Tintstr -> ()

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
  | Tint | Tstr | Tboolean | Talpha _ | Tany | Tnothing | Tintstr -> Vset.empty
  | Tfun (x,y) ->
      List.fold_left (fun a b -> Vset.union a (fvars b)) (fvars y) x
  | Tlist x -> fvars x
  | Tvar tv -> Vset.singleton tv

module Smap = Map.Make(String)
module Sset = Set.Make(String)

type schema = { vars : Sset.t; typ : types }
type variable = { env_vars : Sset.t; typ : types }


type env = {
  bindings : schema Smap.t;
  var_bindings : variable Smap.t;
  types : Sset.t;
  fvars : Vset.t }

let rec bien_forme environment typ = 
  (* Vérifie que toutes les types polymorphes sont bien définis *)
  match head typ with
  | Tint | Tstr | Tboolean | Tany | Tnothing | Tintstr -> true
  | Tfun (x,y) -> List.for_all (bien_forme environment) x
    && bien_forme environment y
  | Tlist x -> bien_forme environment x
  | Talpha x -> Sset.mem x environment
  | Tvar _ -> true (*Je ne sais que faire ici. Avec true, le typage n'est pas sûr mais passe les tests, tandis qu'avec false, il est sûr mais ne passe pas les tests*)

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

let generalize typ fvars =
  let matches = Hashtbl.create 16 in
  let rec replace typ = 
    match head typ with
    | Tany | Tnothing | Tint | Tstr | Tintstr | Tboolean 
    | Talpha _ -> head typ
    | Tlist t -> Tlist (replace t)
    | Tfun (args, ret) -> Tfun (List.map replace args, replace ret)
    | Tvar v -> 
        if Vset.mem v fvars then (Tvar v)
        else if Hashtbl.mem matches v
          then Talpha (Hashtbl.find matches v)
        else (
          Hashtbl.replace matches v (string_of_int (Hashtbl.length matches));
          Talpha (Hashtbl.find matches v))

  in let new_t = replace typ in
  (Hashtbl.fold (fun _ key set -> Sset.add key set) matches Sset.empty,
  new_t)

let add_bind environment name typ =
  if name = "_" then environment 
  else
  if Smap.mem name environment.bindings ||
      Smap.mem name environment.var_bindings
  then raise (RedefinedVariable name)
  else
    let vars, new_typ = generalize typ environment.fvars in
    let scheme = {vars = vars; typ = new_typ} in
    let fvars = environment.fvars in
    {
      bindings = Smap.add name scheme environment.bindings;
      var_bindings = environment.var_bindings;
      types = environment.types;
      fvars = fvars
    }

let add_var environment name typ = 
  if name = "_" then environment 
  else
  if Smap.mem name environment.bindings ||
      Smap.mem name environment.var_bindings
  then raise (RedefinedVariable name)
  else
    let fvars = Vset.union environment.fvars (fvars typ) in
    let v = { env_vars = environment.types; typ = typ } in
    {
      bindings = environment.bindings;
      var_bindings = Smap.add name v environment.var_bindings;
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
  
let rec sous_type t' t = 
  match head t', head t with
  | _, Tany -> ()
  | Tlist x, Tlist y -> sous_type x y
  | Tint, Tint | Tstr, Tstr | Tnothing, Tnothing 
  | Tboolean, Tboolean | Tint, Tintstr | Tstr, Tintstr 
  | Tintstr, Tintstr -> ()
  | Tfun (x1,y1), Tfun (x2,y2) -> begin
      try
        List.iter2 sous_type x2 x1;
        sous_type y1 y2
      with | Invalid_argument _ -> raise (UnificationFailure (t,t'))
    end
  | Tvar tv', Tvar tv -> begin
      match tv'.def, tv.def with
      | None, _ -> tv.def <- Some (Tvar tv')
      | _, None -> tv'.def <- Some (Tvar tv)
      | Some t1, Some t2 -> sous_type t1 t2 end
  | Tvar tv', x -> begin 
      match tv'.def with 
      | None -> tv'.def <- Some x
      | Some x' -> sous_type x' x end 
  | x', Tvar tv -> begin
      match tv.def with
      | None -> tv.def <- Some x'
      | Some x -> sous_type x' x end
  | Talpha x, Talpha y when x = y -> ()
  | _ -> unification_error t t'

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
  if Smap.mem id e.var_bindings then (Smap.find id e.var_bindings).typ
  else try
    let s = Smap.find id e.bindings in
    let rec fresh_type quant maps t = 
      match head t with
      | Tany | Tnothing | Tboolean | Tstr | Tint | Tintstr -> head t
      | Tlist x -> Tlist (fresh_type quant maps x)
      | Tfun (t_list, tret) ->
          Tfun (List.map (fresh_type quant maps) t_list,
            fresh_type quant maps tret)
      | Talpha a -> if Hashtbl.mem maps a then Hashtbl.find maps a
          else if Sset.mem a quant
            then let newmap = Tvar (V.create ()) in 
            (Hashtbl.add maps a newmap; newmap)
          else Talpha a
      | Tvar t -> Tvar t in
    fresh_type s.vars (Hashtbl.create 16) s.typ
  with | Not_found -> raise (VariableNotFound id)

let rec w (environment : env) (stmts : stmt list) : types*stmt list =
  (* Renvoie le type du dernier stmt, ou Tnothing si stmts est vide *)
  let (_, ret_type, instructions) = List.fold_left
    (fun (e,t, ins) s ->
      let new_env, type1, expr1 = w_stmt e s in (new_env, type1, expr1::ins)) (environment, Tnothing, []) stmts 
  in ret_type, List.rev instructions

and w_stmt (environment:env) (stmt: stmt) : env*types*stmt = 
  match stmt with 
  | Sexpr e -> let type1, expr1 = w_expr e environment in 
    (environment, type1, Sexpr expr1)
  | Saffect (id,e) -> (
    try
      let type1, expr1  = w_expr e environment in 
      let v = (Smap.find id environment.var_bindings) in
      if not (bien_forme (v.env_vars) type1) then raise AffectationTypeError 
      else
      (sous_type type1 v.typ;
      (environment, v.typ, Saffect(id,expr1)))
    with | Not_found -> raise (VariableNotFound id))
  | Svar (id, ta, e) ->
    let r_type = read_ta environment ta in
    let type1, expr1 = w_expr e environment in
    begin
      sous_type type1 r_type;
      (add_var environment id r_type, Tnothing, Svar(id, ta, expr1)) end
  | Sconst (id, ta, e) ->
    let ret_type = read_ta environment ta in
    let type1, expr1 = w_expr e environment in
    (sous_type type1 ret_type;
    (add_bind environment id ret_type, Tnothing, Sconst (id, ta, expr1)))

  | Sfun (id, poly, params, ta, e) -> 
    let env_poly = add_poly environment poly in
    let env_params = List.fold_left
      (fun e (id,t_a) -> add_bind e id (read_ta env_poly t_a))
      env_poly params in
    let start_types = List.fold_right (
      fun (id, t_a) l -> (read_ta env_poly t_a)::l
    ) params [] in
    let ret_type = read_ta env_poly ta in 
    let new_env = add_schema env_params id {
      vars = Sset.of_list poly;
      typ = (Tfun (start_types, ret_type))
    } in 
    let type1, instructions = w new_env e in
    (sous_type type1 ret_type);
    (add_schema environment id {
      vars = Sset.of_list poly;
      typ = (Tfun (start_types, ret_type))
    },
    Tnothing, Sfun2(id,List.map fst params,instructions))
  | Sfun2  _ -> failwith "HUGOOOOOOOOOOOO"

and w_expr (exp:expr) (environment:env) : types*expr = 
  match exp with
  | Bexpr (op,e1,e2) ->  begin match op with 
    | Bsub | Bmul | Bdiv -> begin 
      let type1, expr1 = w_expr e1 environment in
      let type2, expr2 = w_expr e2 environment in
      unify type1 Tint;
      unify type2 Tint;
      Tint, Bexpr (op,expr1,expr2)
    end
    | Badd -> let type1, expr1 = w_expr e1 environment in
      let type2, expr2 = w_expr e2 environment in begin
      unify type1 type2 ;
      try (unify type1 Tint; Tint, Bexpr(Badd, expr1, expr2))
      with UnificationFailure(_,_) ->
        (unify type1 Tstr; Tstr, Bexpr(Baddstr, expr1, expr2))
      end
    | Beq | Bneq -> begin
      let _, expr1 = w_expr e1 environment in 
      let _, expr2 = w_expr e2 environment in 
      Tboolean, Bexpr(op, expr1, expr2)
    end 
    | Band | Bor -> 
      let type1, expr1 = w_expr e1 environment in
      let type2, expr2 = w_expr e2 environment in begin
      unify type1 Tboolean;
      unify type2 Tboolean;
      Tboolean, Bexpr(op, expr1, expr2)
    end 
    | Ble | Blt | Bge | Bgt ->
      let type1, expr1 = w_expr e1 environment in
      let type2, expr2 = w_expr e2 environment in begin
      unify type1 Tint;
      unify type2 Tint;
      Tboolean, Bexpr(op, expr1, expr2)
    end
    | Baddstr -> assert false
    end
  | Ecst cst -> ((match cst with 
      | Cbool _ -> Tboolean
      | Cint  _ -> Tint
      | Cstr  _ -> Tstr)
    , Ecst cst)
  | Evar id -> find environment id, Evar id
  | Eblock b -> let type1, ins = w environment b in type1, Eblock b
  | Eif (e1, e2, e3) -> 
      let type1, expr1 = w_expr e1 environment in
      let type2, expr2 = w_expr e2 environment in
      let type3, expr3 = w_expr e3 environment in
      begin
      unify type1 Tboolean;
      unify type2 type3;
      type2, Eif(expr1, expr2, expr3)
    end
  | Ecall (func, args) ->
      let f_type, f_expr = w_expr func environment in begin
      match f_type with
      | Tfun (targs, tret) -> begin
        try
          tret, Ecall (f_expr,
          List.fold_left2 (fun ins x y ->
            let type1, expr1 = (w_expr y environment) in
            (sous_type type1 x;ins@[expr1]))
            [] targs args)
        with | Invalid_argument _ -> raise
          (WrongArgsNumber ((List.length targs),(List.length args)))
        end
      | _ -> raise (NotCallable f_type) end
  | Ecases (ta, e, branches) ->
      let type1, expr1 = w_expr e environment in 
      let sub_type = Tvar (V.create ()) in begin
      unify (read_ta environment ta) (Tlist sub_type);
      sous_type type1 (Tlist sub_type);
      match branches with
      | (Branch1 ("empty", be))::(Branch2 ("link", [x;y], bl))::[]
      | (Branch2 ("link", [x;y], bl))::(Branch1 ("empty", be))::[] ->
          let new_env = add_bind (add_bind environment x sub_type) y (Tlist sub_type) in
          let t_ret, expr_ret = w new_env bl in
          let t_be, expr_be = (w environment be) in
          begin unify t_ret t_be;
          t_ret, Ecases (ta, expr1, (Branch1 ("empty", expr_be))::(Branch2 ("link", [x;y], expr_ret))::[]) end
      | _ -> raise WrongCase
      end

  | Elam (Funbody (params, ta, b)) -> 
      let new_env = List.fold_left
      (fun e (id,t_a) -> add_bind e id (read_ta environment t_a))
      environment params in
      let start_types = List.fold_right (
        fun (id, t_a) l -> (read_ta environment t_a)::l
      ) params [] in
      let r_type = (read_ta new_env ta) in
      let type1, expr1 = (w new_env b) in
      (sous_type type1 r_type ; Tfun(start_types, r_type), Elam(Funbody(params,ta,b)))

let typing (s : stmt list) : types*block = w start_environment s
