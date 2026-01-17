
open Format
open X86_64
open Ast

(* phase 1 : allocation des variables *)

exception VarUndef of string

let (genv : (string, unit) Hashtbl.t) = Hashtbl.create 17

let counter = ref 20

let new_label () = 
  counter := !counter + 1 ;
  "l" ^ (string_of_int !counter)


let extract_length (s : astmt) : frame_size =
  match s with 
  | Aexpr(_,i) -> i
  | Aaffect(_,_,i) -> i
  | Avar(_,_,i) -> i
  | Aconst (_,_,i) -> i 
  | Afun(_,_,_,i,_,_) -> i


let rec get_vars_expr (env:local_env) (fermeture:local_env) fp e = 
  match e with 
  | Ecst i -> fermeture, fp
  | Evar id when id = "print" -> fermeture, fp
  | Evar id -> if Smap.mem id env then fermeture, fp
    else if Smap.mem id fermeture then fermeture, fp
    else Smap.add id fp fermeture, fp + 1
  | Bexpr (b,e1,e2) -> 
      let new_ferm, new_fp = get_vars_expr env fermeture fp e1 in 
      get_vars_expr env new_ferm new_fp e2
  | Eblock(ins) -> get_vars_block env fermeture fp ins 
  | Ecall(e, args) -> 
      List.fold_left (fun (ferm, fp) e -> get_vars_expr env ferm fp e)
        (get_vars_expr env fermeture fp e) args
  | Eif(e1,e2,e3) -> 
    let new_ferm, new_fp = get_vars_expr env fermeture fp e1 in 
    let new_ferm2, new_fp2 = get_vars_expr env new_ferm new_fp e2 in 
    get_vars_expr env new_ferm2 new_fp2 e3
  | Elam _ | Ecases _ -> failwith "not supported"

and get_vars_stmt env fermeture fp s = match s with
  | Sexpr e -> let new_ferm, new_fp = get_vars_expr env fermeture fp e in
      env, new_ferm, new_fp
  | Sconst (id, _, e) | Svar (id,_,e) -> 
      let new_ferm, new_fp = get_vars_expr env fermeture fp e in
      Smap.add id 0 env, new_ferm, new_fp
  | Saffect(id,e) -> 
      let new_ferm, new_fp = get_vars_expr env fermeture fp e in
      env, new_ferm, new_fp
  | Sfun2 (id, args, ins) -> 
      let new_env = List.fold_left (fun env arg -> Smap.add arg 0 env)
        (Smap.add id 0 env) args in
      let fermeture, fp = get_vars_block new_env fermeture fp ins
      in Smap.add id 0 env, fermeture, fp
  | Sfun _ -> assert false

and get_vars_block env fermeture fp ins =
  let _, ferm, fp = List.fold_left (fun (env, fermeture, fp) s -> 
    get_vars_stmt env fermeture fp s) (env, fermeture, fp) ins
  in ferm, fp
  


let rec alloc_expr (env: local_env) (fermeture: local_env) (fpcur: int) e = 
  (* print_expr e ;
  print_newline(); *)
  match e with 
  | Ecst i -> Acst i, fpcur
  | Evar id -> (
    try 
      Aident (Smap.find id env), fpcur
    with | Not_found ->
    try 
        Aferm (Smap.find id fermeture), fpcur
    with | Not_found ->
      raise (VarUndef id))
  | Bexpr (b, e1, e2) -> 
    let exp1, s1 = alloc_expr env fermeture fpcur e1 in
    let exp2, s2 = alloc_expr env fermeture (fpcur + 1) e2 in
    Abexpr (b, exp1, exp2), fpcur

  | Eblock(instructions) -> 
    (* let (i,l') = List.fold_left (fun (i, tl) e -> let e',j = alloc_expr env fpcur e in max i j, e' :: tl) (fpcur,[]) l in 
    Ablock(l'),i *)
    let (code, new_fpcur) = alloc_block instructions env fermeture fpcur  in 
    (*
    let i' = List.fold_left (fun acc s -> max acc (extract_length s)) 0 code in *)
    (Ablock(code),fpcur)
    
  | Ecall (expr, [args]) when expr = Evar "print" -> 
    Aprint (fst (alloc_expr env fermeture fpcur args)), 0
  (*| Ecall (expr, args) -> Acall (fst (alloc_expr env fpcur expr),
      List.map (fun x -> fst (alloc_expr env fpcur x)) args), fpcur
  *)

  | Ecall (f, l) ->
    (* Créée un bug car f est une expression *)
    (* TODO : comprendre Ecall *)
    (* Comme les fonctions sont des valeurs de première classe,
      elles peuvent être calculée/définies comme fonctions anonymes
      avant d'être appelées*)
    let l, s = List.fold_right (fun e (le, s) -> 
      let e', s' = alloc_expr env fermeture fpcur e in (e'::le, max s s')
    ) l ([],fpcur) in 
    let f', i = alloc_expr env fermeture fpcur f in 
    Acall (f',l), max i s 

  | Eif (e1, e2, e3) -> 
    let e1', i1 = alloc_expr env fermeture fpcur e1 in
    let e2', i2 = alloc_expr env fermeture fpcur e2 in
    let e3', i3 = alloc_expr env fermeture fpcur e3 in
    Aif(e1', e2', e3'), fpcur
  | _ -> failwith "alloc_expr - cas non traite"

and alloc_stmt (env: local_env) fermeture (fpcur: int) = function 
  | Sexpr e ->
      let new_expr, fpcur = alloc_expr env fermeture fpcur e in
      (Aexpr (new_expr,0), fpcur), env
  | Sconst (id, ta, e) | Svar (id,ta,e) -> 
      let new_expr, fpcur = alloc_expr env fermeture fpcur e in
      (Aconst (id, new_expr, fpcur), fpcur + 1), Smap.add id fpcur env
  | Saffect (id, e) -> 
      let new_expr, fpcur = alloc_expr env fermeture fpcur e in
      (Aaffect (Smap.find id env, new_expr, fpcur), fpcur), env
  | Sfun2 (ident, args, ins) -> 
      let new_env, _ = List.fold_left (fun (map, i) id -> 
        (Smap.add id i map, i-1)) (Smap.empty, -3) args in
      let new_fermeture, taille = get_vars_block new_env Smap.empty 0 ins in
      let new_expr, fpcur2 = alloc_block ins new_env new_fermeture 1 in 
      let ferm_comp = Smap.fold 
        (fun s i map -> Fmap.add i (fst (alloc_expr (Smap.add ident fpcur env) fermeture 0 (Evar s))) map) new_fermeture Fmap.empty in 
      (Afun(ident, args,new_expr, 0 , ferm_comp, taille), fpcur + 1), Smap.add ident fpcur env
  | Sfun _ -> failwith "HUGOOOO" 
  (* | _ -> failwith "alloc_stmt - cas non traité" *)

and alloc_block instructions (env: local_env) fermeture (fpcur: int) = 
    (* Le nouvel environnement n'est pas renvoyé *)
    (*puisque toutes les variables déclarées ne vivent que dans le bloc *)
    let statements, fpcur, _ =
      List.fold_left (
      fun (stmts, fpcur, env) stmt ->
        let (new_stmt, new_fpcur), new_env = (alloc_stmt env fermeture fpcur stmt) in 
        (new_stmt::stmts), new_fpcur, new_env) ([], fpcur, env) instructions
    in (List.rev statements), fpcur

(* and alloc env fpcur = List.fold_left (alloc_stmt env fpcur)  *)

let popn n = addq (imm n) !%rsp
let pushn n = subq (imm n) !%rsp

let rec compile_expr = function 
  | Aident i -> 
      movq (ind ~ofs:(i*(-8)) rbp) !%rax
  | Aferm i -> 
      movq (ind ~ofs:16 rbp) !%rdi ++
      movq (ind ~ofs:(i*8) rdi) !%rax
  | Acst c ->
    (match c with 
    | Cint i ->
        movq (imm 16) !%rdi ++
        call "my_malloc" ++
        movq (imm 2) (ind rax) ++
        movq (imm i) (ind ~ofs:8 rax)
    | Cbool i ->
        movq (imm 16) !%rdi ++
        call "my_malloc" ++
        movq (imm 1) (ind rax) ++
        (if i then movq (imm 1) !%rdi else movq (imm 0) !%rdi) ++
        movq !%rdi (ind ~ofs:8 rax)
    | Cstr s ->
        let n = String.length s in
        movq (imm (16 + n + 1)) !%rdi ++
        call "my_malloc" ++
        fst (String.fold_left (fun (ins,i) c ->
          ins ++ movb (imm (Char.code c)) (ind ~ofs:i rax), i+1)
          (movq (imm 3) (ind rax), 16) s) ++
        movq (imm n) (ind ~ofs:8 rax) ++
        movb (imm 0) (ind ~ofs:(16+n) rax))
  
  | Abexpr (b,e1,e2) when b = Baddstr -> 
      compile_expr e1 ++
      pushq !%rax ++
      compile_expr e2 ++
      movq !%rax !%rdi ++
      popq rax ++
      movq (ind ~ofs:8 rdi) !%rdx ++
      movq (ind ~ofs:8 rax) !%rsi ++
      pushq !%rdi ++
      pushq !%rax ++
      movq !%rdx !%rdi ++
      addq !%rsi !%rdi ++
      pushq !%rdi ++
      addq (imm 17) !%rdi ++
      call "my_malloc" ++
      movq (imm 3) (ind rax) ++
      popq rdi ++
      movq !%rdi (ind ~ofs:8 rax) ++
      popq rdx ++
      movq (imm 16) !%rcx ++
      movq (imm 16) !%rdi ++
      label "1" ++
      cmpb (imm 0) (ind ~index:rdi rdx) ++
      je "1f" ++
      movb (ind ~index:rdi rdx) !%sil ++
      movb !%sil (ind ~index:rcx rax) ++
      incq !%rcx ++
      incq !%rdi ++
      jmp "1b" ++
      label "1" ++
      popq rdx ++
      movq (imm 16) !%rdi ++
      label "1" ++
      cmpb (imm 0) (ind ~index:rdi rdx) ++
      je "1f" ++
      movb (ind ~index:rdi rdx) !%sil ++
      movb !%sil (ind ~index:rcx rax) ++
      incq !%rcx ++
      incq !%rdi ++
      jmp "1b" ++
      label "1" ++
      movb (imm 0) (ind ~index:rcx rax)

  | Abexpr (b, e1, e2) when b = Bor ->
      movq (imm 16) !%rdi ++
      call "my_malloc" ++
      movq (imm 1) (ind rax) ++
      pushq !%rax ++
      compile_expr e1 ++
      cmpq (imm 1) (ind ~ofs:8 rax ) ++
      je "1f" ++
      compile_expr e2 ++
      cmpq (imm 1) (ind ~ofs:8 rax ) ++
      je "1f" ++
      movq (imm 0) !%rdi ++
      jmp "2f" ++
      label "1" ++
      movq (imm 1) !%rdi ++
      label "2" ++
      popq rax ++
      movq !%rdi (ind ~ofs:8 rax)

  | Abexpr (b, e1, e2) when b = Band ->
      movq (imm 16) !%rdi ++
      call "my_malloc" ++
      movq (imm 1) (ind rax) ++
      pushq !%rax ++
      compile_expr e1 ++
      cmpq (imm 0) (ind ~ofs:8 rax ) ++
      je "1f" ++
      compile_expr e2 ++
      cmpq (imm 0) (ind ~ofs:8 rax ) ++
      je "1f" ++
      movq (imm 1) !%rdi ++
      jmp "2f" ++
      label "1" ++
      movq (imm 0) !%rdi ++
      label "2" ++
      popq rax ++
      movq !%rdi (ind ~ofs:8 rax)
  
  | Abexpr (b, e1, e2) when b == Beq ->
    compile_expr e1 ++
    pushq !%rax ++
    compile_expr e2 ++
    popq rdx ++
    movq (ind rax) !%rsi ++
    movq (ind rdx) !%rdi ++
    cmpq !%rsi !%rdi ++
    jne "8f" ++
    cmpq (imm 0) !%rsi ++
    je "9f" ++
    cmpq (imm 4) !%rsi ++
    je "9f" ++
    cmpq (imm 1) !%rsi ++
    je "1f" ++
    cmpq (imm 2) !%rsi ++
    je "1f" ++
    movq (imm 60) !%rax ++ (*TODO: le reste*)
    movq (imm 2) !%rdi ++
    syscall ++
    label "1" ++
    movq (ind ~ofs:8 rax) !%rsi ++
    movq (ind ~ofs:8 rdx) !%rdi ++
    cmpq !%rsi !%rdi ++
    je "9f" ++
    label "8" ++
    pushq (imm 0) ++
    jmp "7f" ++
    label "9" ++
    pushq (imm 1) ++
    label "7" ++
    movq (imm 16) !%rdi ++
    call "my_malloc" ++
    movq (imm 1) (ind rax) ++
    popq rdi ++
    movq !%rdi (ind ~ofs:8 rax)

  | Abexpr (b, e1, e2) when b == Bneq ->
    compile_expr e1 ++
    pushq !%rax ++
    compile_expr e2 ++
    popq rdx ++
    movq (ind rax) !%rsi ++
    movq (ind rdx) !%rdi ++
    cmpq !%rsi !%rdi ++
    jne "8f" ++
    cmpq (imm 0) !%rsi ++
    je "9f" ++
    cmpq (imm 4) !%rsi ++
    je "9f" ++
    cmpq (imm 1) !%rsi ++
    je "1f" ++
    cmpq (imm 2) !%rsi ++
    je "1f" ++
    exit 2 ++ (*TODO: le reste*)
    label "1" ++
    movq (ind ~ofs:8 rax) !%rsi ++
    movq (ind ~ofs:8 rdx) !%rdi ++
    cmpq !%rsi !%rdi ++
    je "9f" ++
    label "8" ++
    pushq (imm 1) ++
    jmp "7f" ++
    label "9" ++
    pushq (imm 0) ++
    label "7" ++
    movq (imm 16) !%rdi ++
    call "my_malloc" ++
    movq (imm 1) (ind rax) ++
    popq rdi ++
    movq !%rdi (ind ~ofs:8 rax)

  | Abexpr (b, e1, e2) -> begin
      compile_expr e1 ++
      pushq (ind ~ofs:8 rax) ++
      compile_expr e2 ++
      movq (ind ~ofs:8 rax) !%rdx ++
      popq rax ++
      begin 
        match b with
        | Beq | Bneq | Blt | Ble | Bgt | Bge | Band | Bor -> 
            (match b with 
            | Beq
            | Bneq
            | Blt
            | Ble
            | Bgt
            | Bge -> 
              (*TODO: ne pas vérifier uniquement la valeur pour l'égalité, mais aussi le type*)
              let l = new_label()  in
              let l2 = new_label() in
              cmpq !%rdx !%rax ++ 
              (match b with 
              | Beq -> jne 
              | Bneq -> je 
              | Blt -> jge
              | Ble -> jg
              | Bgt -> jle
              | Bge -> jl 
              | _ -> assert(false)) l
              ++ movq (imm 1) !%rax
              ++ jmp l2
              ++ label l
              ++ movq (imm 0) !%rax
              ++ label l2
            | _ -> assert false
            ) ++ 
            pushq !%rax ++
            movq (imm 16) !%rdi ++
            call "my_malloc" ++
            movq (imm 1) (ind rax) ++
            popq rdi ++
            movq !%rdi (ind ~ofs:8 rax)
            
        | Badd | Bsub | Bmul | Bdiv -> 
            (match b with 
            | Badd -> addq !%rdx !%rax
            | Bsub -> subq !%rdx !%rax
            | Bmul -> imulq !%rdx !%rax
            | Bdiv -> movq !%rdx !%rbx ++
                movq (imm 0) !%rdx ++
                idivq !%rbx 
                (* Si j'ai bien compris ça marche *)
            | _ -> assert false
            ) ++
          pushq !%rax ++
          movq (imm 16) !%rdi ++
          call "my_malloc" ++
          movq (imm 2) (ind rax) ++
          popq rdi ++
          movq !%rdi (ind ~ofs:8 rax)

        (* | _ -> failwith "pas traité" *)
        | Baddstr -> assert false
        end
    end
  | Aprint i ->
      compile_expr i ++
      movq !%rax !%rdi ++
      call "print"

  | Aif(e1,e2,e3) -> 
    (* TODO : À modifier avec le nouveau type de données *)
    compile_expr e1 ++ 
    (* cmpq (imm 0) !%rax ++  *)
    cmpq (imm 0) (ind ~ofs:8 rax) ++
    jne "1f" ++
    compile_expr e3 ++
    jmp "2f" ++
    label "1" ++
    compile_expr e2 ++ 
    label "2"

  | Ablock(b) -> compile_block b
  | Acall (f, args) -> 
      let code = ref (List.fold_left (fun c e -> 
      compile_expr e ++
      pushq !%rax ++ c) nop args ++
      compile_expr f ++
      addq (imm 16) !%rax ++
      pushq !%rax ++
      call_star (ind ~ofs:(-8) rax))
      in (
        for i=0 to List.length args do 
          code := !code ++ popq rdi
        done; !code
      )
      

  | _ -> failwith "compile_expr - cas non traité"

and compile_stmt = function 
  | Aexpr (e, _) -> compile_expr e
  | Aconst (_, e, _) -> 
      compile_expr e ++
      pushq !%rax
  | Aaffect (i, e, _) -> 
      compile_expr e ++
      (*TODO on doit pouvoir free ici*)
      movq !%rax (ind ~ofs:(-8*i) rbp)

  | Afun (id, args, ins, _, fermeture, taille) -> 
      let base =
      let fin = new_label () in
      let nom = new_label () in
      jmp fin ++
      label nom ++
      pushq !%rbp ++
      movq !%rsp !%rbp ++
      compile_block ins ++
      movq !%rbp !%rsp ++
      popq rbp ++
      ret ++
      label fin ++
      movq (imm (16 + 8*taille)) !%rdi ++
      call "my_malloc" ++
      movq (imm 6) (ind rax) ++
      leaq (lab nom) rdx ++
      movq !%rdx (ind ~ofs:8 rax) ++
      pushq !%rax
      in 
      Fmap.fold (fun i e code ->
        code ++
        compile_expr e ++
        popq rdi ++
        movq !%rax (ind ~ofs:(16 + 8*i) rdi) ++
        pushq !%rdi
      ) fermeture base

  | _ -> failwith "compile_stmt - cas non traité"

and compile_block instructions = 
  List.fold_left (fun c s -> c ++ compile_stmt s) nop instructions










































(*

let extract_length (s : astmt) : frame_size =
  match s with 
  | Aexpr(_,i) -> i
  | Aaffect(_,_,i) -> i
  | Avar(_,_,_,i) -> i
  | Aconst (_,_,_,i) -> i 
  | Afun(_,_,_,_,_,i) -> i
 

let rec alloc_expr (env: local_env) (fpcur: int) = function
  | Ecst c ->
    Acst c, fpcur

  | Evar x -> (match Smap.find_opt x env with
    | Some i -> Lvar i, fpcur
    | None -> if not (Hashtbl.mem genv x) then raise (VarUndef x)
        else Gvar x, fpcur)

  | Bexpr (o, e1, e2)->
    let exp1, s1 = alloc_expr env fpcur e1 in
    let exp2, s2 = alloc_expr env fpcur e2 in
    ABexpr (o, exp1, exp2), max s1 s2

  | Ecall (f, l) ->
    (* Créée un bug car f est une expression *)
    (* TODO : comprendre Ecall *)
    (* Comme les fonctions sont des valeurs de première classe,
      elles peuvent être calculée/définies comme fonctions anonymes
      avant d'être appelées*)
    let l, s = List.fold_right (fun e (le, s) -> 
      let e', s' = alloc_expr env fpcur e in (e'::le, max s s')
    ) l ([],fpcur) in 
    let f', i = alloc_expr env fpcur f in 
    Acall (f',l), max i s 

  | Eblock(instructions) -> 
    (* let (i,l') = List.fold_left (fun (i, tl) e -> let e',j = alloc_expr env fpcur e in max i j, e' :: tl) (fpcur,[]) l in 
    Ablock(l'),i *)
    let code = alloc instructions in 
    let i' = List.fold_left (fun acc s -> max acc (extract_length s)) 0 code in 
    (Ablock(code),i' + fpcur)
  
  | Eif (e1, e2, e3) -> 
    let e1', i1 = alloc_expr env fpcur e1 in
    let e2', i2 = alloc_expr env fpcur e2 in
    let e3', i3 = alloc_expr env fpcur e3 in
    Aif(e1', e2', e3'), max (max i1 i2) i3 
    
  | Ecases(t, e, l) -> begin 
    (* Si la liste a passé le parsing, 
      alors elle est composée de deux éléments qui sont empty et list(x,y)*)
    let e1,i1 = alloc_expr env fpcur e in 
    match l with 
    | (Branch1 ("empty", be))::(Branch2 ("link", [x;y], bl))::[]
    | (Branch2 ("link", [x;y], bl))::(Branch1 ("empty", be))::[] ->
      let ((Ablock b2),i2) = alloc_expr env fpcur (Eblock be) in 
      let ((Ablock b3),i3) = alloc_expr env fpcur (Eblock  bl) in 
      (* +16 : affectation des variables x et y *)
      (Acases(t,e1,[Branch1 ("empty", b2); Branch2 ("link", [x;y], b3)]), 
      max i1 (max i2 (i3 + 16)))
    | _ -> assert false
  end
  | Elam (Funbody(l_args, typ_out, instructions)) ->
    (* let (i,l') = List.fold_left (fun (i, tl) e -> 
      let e',j = alloc_expr env fpcur e in max i j, e' :: tl
    ) (fpcur,[]) l_args in  *)
    let code = alloc instructions in 
    let i' = List.fold_left (fun acc s -> max acc (extract_length s)) 0 code in 
    (* max i (i' + fpcur) *)
    (Alam(Afunbody(l_args, typ_out, code)),i' + fpcur)

    
  | _ -> failwith "pas traité"
  (* 

  | PLetin (x, e1, e2) ->
    let exp1, s1 = alloc_expr env fpcur e1 in 
    let exp2, s2 = alloc_expr (Smap.add x (-(fpcur + 8)) env) (fpcur+8) e2 in 
    Letin (-(fpcur + 8), exp1, exp2), max s1 s2
*)



and alloc_stmt = function
  (* Il y a des blocks dans les expressions *)
  (* -> Comment savoir si une variable est locale ou globale ? *)
  (* Solutions : *)
  (* - créer deux fonctions distinctes *)
  (* - Traiter la table des variables globales comme un environnement local *)



(* | Sexpr of expr
| Saffect of ident * expr
| Svar of ident * type_annotation * expr
| Sconst of ident * type_annotation * expr 
| Sfun of ident * ident list * param list * type_annotation * block

 *)

  | Saffect (x, e) ->
  let e, s = alloc_expr Smap.empty 0 e in
  (Hashtbl.add genv x () ; Set (x, e, s))
  (* 

  | PFun (f, l, e) ->
    let new_env, _ = List.fold_left (fun (env,i) s -> (Smap.add s (8*i) env, i+1))
    (Smap.empty, 2) l in 
    let e', s' = alloc_expr new_env 0 e in Fun (f, e', s')

  | PPrint e ->
    let e, fpmax = alloc_expr Smap.empty 0 e in
    Print (e, fpmax) *)

and alloc = List.map alloc_stmt

(******************************************************************************)
(* phase 2 : production de code *)

let popn n = addq (imm n) !%rsp
let pushn n = subq (imm n) !%rsp

let rec compile_expr = function
  | Acst i ->
    (* C'est quoi imm ? *)
      pushq (imm i)

  | LVar fp_x ->
      pushq (ind ~ofs:fp_x rbp)

  | GVar x ->
      pushq (lab x)

  | Binop (o, e1, e2)->
      compile_expr e1 ++
      compile_expr e2 ++
      popq rbx ++ popq rax ++
      (match o with
        | Add -> addq !%rbx !%rax
        | Sub -> subq !%rbx !%rax
        | Mul -> imulq !%rbx !%rax
        | Div -> cqto ++ idivq !%rbx) ++
       pushq !%rax

  | Letin (ofs, e1, e2) ->
      compile_expr e1 ++
      popq rax ++ movq !%rax (ind ~ofs rbp) ++
      compile_expr e2

  | Call (f, l) ->
      List.fold_right (fun e c -> compile_expr e ++ c) l nop ++
      call f ++
      popn (8*List.length l) ++
      pushq !%rax

let compile_stmt (codefun, codemain) = function
  | Set (x, e, fpmax) ->
    let code =
      pushn fpmax ++
      compile_expr e ++
      popq rax ++ movq !%rax (lab x) ++
      popn fpmax
    in
    codefun, codemain ++ code

  | Fun (f, e, fpmax) ->
    let code = 
      label f ++
      pushq !%rbp ++
      movq !%rsp !%rbp ++
      pushn fpmax ++ 
      compile_expr e ++
      popq rax ++
      popn fpmax ++
      popq rbp ++
      ret 
    in
    code ++ codefun, codemain


  | Print (e, fpmax) ->
    let code =
      pushn fpmax ++
      compile_expr e ++
      popq rdi ++
      popn fpmax ++
      call "print_int"
    in
    codefun, codemain ++ code
*)
let compile_program p ofile =
  let start_env = Smap.add "nothing" 2 (Smap.singleton "num-modulo" 1) in
  let start_fpcur = 3 in (* 1 pour num_modulo et 2 pour nothing*)
  let p, _ = alloc_block p start_env Smap.empty start_fpcur in
  let code = List.fold_left (fun c s -> c ++ compile_stmt s ) nop p in
  let p =
    { text =
        globl "main" ++ label "main" ++
        pushq !%rbp ++
        movq !%rsp !%rbp ++
      (* On alloue num_modulo *)
        movq (imm 16) !%rdi ++
        call "my_malloc" ++
        movq (imm 6) (ind rax) ++
        leaq (lab "num_modulo") rdx ++
        movq !%rdx (ind ~ofs:8 rax) ++
        pushq !%rax ++
      (* On alloue nothing *)
        movq (imm 8) !%rdi ++
        call "my_malloc" ++
        movq (imm 0) (ind rax) ++
        pushq !%rax ++

      (* Le code compilé est rajouté ici *)
        code ++


        movq (imm 0) !%rax ++ (* exit *)
        movq !%rbp !%rsp ++
        popq rbp ++
        ret ++
        label "my_malloc" ++
        pushq !%rbp ++
        movq !%rsp !%rbp ++
        (*andq (imm (-16)) !%rsp ++*)
        (*movq (ind ~ofs:24 rbp) !%rdi ++*)
        call "malloc" ++
        movq !%rbp !%rsp ++
        popq rbp ++ 
        ret ++
        label "print" ++
        pushq !%rbp ++
        pushq !%rdi ++
        movq !%rsp !%rbp ++
        (*andq (imm (-16)) !%rsp ++*)
        cmpq (imm 0) (ind rdi) ++
        je "0f" ++
        cmpq (imm 1) (ind rdi) ++
        je "1f" ++
        cmpq (imm 2) (ind rdi) ++
        je "2f" ++
        cmpq (imm 3) (ind rdi) ++
        je "3f" ++
        jmp "7f" ++
        label "0" ++
        movq (ilab ".nothing") !%rdi++
        movq (imm 0) !%rax ++
        call "printf" ++
        jmp "7f" ++
        label "1" ++
        cmpq (imm 1) (ind ~ofs:8 rdi) ++
        je "print_true" ++
        movq (ilab ".false") !%rdi ++
        movq (imm 0) !%rax ++
        call "printf" ++
        jmp "7f" ++
        label "print_true" ++
        movq (ilab ".true") !%rdi ++
        movq (imm 0) !%rax ++
        call "printf" ++
        jmp "7f" ++
        label "2" ++
        movq (ind ~ofs:8 rdi) !%rsi ++
        movq (ilab ".Sprint_int") !%rdi ++
        movq (imm 0) !%rax ++
        call "printf" ++
        jmp "7f" ++
        label "3" ++
        addq (imm 16) !%rdi ++
        movq !%rdi !%rsi ++
        movq (ilab ".Sprint_str") !%rdi ++
        movq (imm 0) !%rax ++
        call "printf" ++
        jmp "7f" ++
        label "7" ++
        popq rax ++
        popq rbp ++
        ret ++
        label "num_modulo" ++
        pushq !%rbp ++
        movq !%rsp !%rbp ++
        movq (ind ~ofs:24 rbp) !%rsi ++
        movq (ind ~ofs:8 rsi) !%rax ++
        movq (ind ~ofs:32 rbp) !%rsi ++
        movq (ind ~ofs:8 rsi) !%rdx ++
        movq (imm 0) !%rsi ++
        cmpq (imm 0) !%rdx ++
        jg "1f" ++
        movq (imm 1) !%rsi ++
        negq !%rdx ++
        negq !%rax ++
        label "1" ++
        cmpq (imm 0) !%rax ++
        jg "1f" ++
        addq !%rdx !%rax ++
        jmp "1b" ++
        label "1" ++
        cmpq !%rax !%rdx ++
        jg "1f" ++
        subq !%rdx !%rax ++
        jmp "1b" ++
        label "1" ++
        cmpq (imm 0) !%rsi ++
        je "1f" ++
        negq !%rax ++
        label "1" ++
        pushq !%rax ++
        movq (imm 16) !%rdi ++
        call "my_malloc" ++
        popq rdi ++
        movq (imm 2) (ind rax) ++
        movq !%rdi (ind ~ofs:8 rax) ++
        popq rbp ++
        ret ;

        
      data =
        Hashtbl.fold (fun x _ l -> label x ++ dquad [1] ++ l) genv
          (label ".Sprint_int" ++ string "%d" ++
          label ".Sprint_str" ++ string "%s" ++
          label ".true" ++ string "true" ++
          label ".false" ++ string "false" ++
          label ".nothing" ++ string "nothing")
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
  fprintf fmt "@?";
  close_out f
