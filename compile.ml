
open Format
open X86_64
open Ast

(* phase 1 : allocation des variables *)

exception VarUndef of string

let (genv : (string, unit) Hashtbl.t) = Hashtbl.create 17

module Smap = Map.Make(String)

type local_env = int Smap.t

let counter = ref 20

let new_label () = 
  counter := !counter + 1 ;
  "l" ^ (string_of_int !counter)

let rec alloc_expr (env: local_env) (fpcur: int) e = 
  print_expr e ;
  match e with 
  | Ecst i -> Acst i, fpcur
  | Evar id -> (
    try 
      Aident (Smap.find id env), fpcur
    with | Not_found -> raise (VarUndef id))
  | Bexpr (b, e1, e2) -> 
    let exp1, s1 = alloc_expr env fpcur e1 in
    let exp2, s2 = alloc_expr env (fpcur + 1) e2 in
    Abexpr (b, exp1, exp2), fpcur
  | Ecall (expr, [args]) when expr = Evar "print" -> Aprint (fst (alloc_expr env fpcur args)), 0
  (*| Ecall (expr, args) -> Acall (fst (alloc_expr env fpcur expr),
      List.map (fun x -> fst (alloc_expr env fpcur x)) args), fpcur
  *)
  | Eif (e1, e2, e3) -> 
    let e1', i1 = alloc_expr env fpcur e1 in
    let e2', i2 = alloc_expr env fpcur e2 in
    let e3', i3 = alloc_expr env fpcur e3 in
    Aif(e1', e2', e3'), max (max i1 i2) i3 
  | _ -> failwith "alloc_expr - cas non traite"

and alloc_stmt (env: local_env) (fpcur: int) = function 
  | Sexpr e -> let new_expr, fpcur = alloc_expr env fpcur e in
    (Aexpr (new_expr,0), fpcur), env

  | _ -> failwith "alloc_stmt - cas non traité"

and alloc_block instructions (env: local_env) (fpcur: int) = 
    let statements, fpcur, _ =
      List.fold_left (
      fun (stmts, fpcur, env) stmt ->
        let (new_stmt, new_fpcur), new_env = (alloc_stmt env fpcur stmt) in 
        new_stmt::stmts, new_fpcur, new_env) ([], fpcur, env) instructions
    in (List.rev statements), fpcur


let popn n = addq (imm n) !%rsp
let pushn n = subq (imm n) !%rsp

let rec compile_expr = function 
  | Acst c ->
    (match c with 
    | Cint i ->
        movq (imm 16) !%rdi ++
        call "my_malloc" ++
        movq (imm 2) !%rdi ++
        movq !%rdi (ind rax) ++
        movq (imm i) !%rdi ++
        movq !%rdi (ind ~ofs:8 rax)
    | Cbool i ->
        movq (imm 16) !%rdi ++
        call "my_malloc" ++
        movq (imm 1) !%rdi ++
        movq !%rdi (ind rax) ++
        if i then movq (imm 1) !%rdi else movq (imm 0) !%rdi ++
        movq !%rdi (ind ~ofs:8 rax)
    | Cstr s ->
        let n = String.length s in
        movq (imm (8 + n + 1)) !%rdi ++
        call "my_malloc" ++
        fst (String.fold_left (fun (ins,i) c ->
          ins ++ movb (imm (Char.code c)) (ind ~ofs:i rax), i+1)
          (movq (imm 3) (ind rax), 8) s) ++
        movb (imm 0) (ind ~ofs:(8+n) rax))

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
            | Band -> andq !%rdx !%rax
            | Bor -> orq !%rdx !%rax
            | _ -> assert false
            ) ++ 
            pushq !%rax ++
            movq (imm 16) !%rdi ++
            call "my_malloc" ++
            movq (imm 1) !%rdi ++
            movq !%rdi (ind rax) ++
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
          movq (imm 2) !%rdi ++
          movq !%rdi (ind rax) ++
          popq rdi ++
          movq !%rdi (ind ~ofs:8 rax)

        (* | _ -> failwith "pas traité" *)
        | Baddstr -> failwith "pas traité" 
        end
    end
  | Aprint i ->
      compile_expr i ++
      movq !%rax !%rdi ++
      call "print"

  | Aif(e1,e2,e3) -> 
    (* TODO : À modifier avec le nouveau type de données *)
    compile_expr e1 ++ 
    cmpq (imm 0) !%rax ++
    jne "1f" ++
    compile_expr e3 ++
    jmp "2f" ++
    label "1" ++
    compile_expr e2 ++ 
    label "2"

  | _ -> failwith "compile_expr - cas non traité"

and compile_stmt = function 
  | Aexpr (e, _) -> compile_expr e
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
  let start_env = Smap.empty in
  let p, _ = alloc_block p start_env 0 in
  let code = List.fold_left (fun c s -> c ++ compile_stmt s ) nop p in
  let p =
    { text =
        globl "main" ++ label "main" ++
        movq !%rsp !%rbp ++
        code ++
        movq (imm 0) !%rax ++ (* exit *)
        ret ++
        label "my_malloc" ++
        pushq !%rbp ++
        movq !%rsp !%rbp ++
        andq (imm (-16)) !%rsp ++
        (*movq (ind ~ofs:24 rbp) !%rdi ++*)
        call "malloc" ++
        movq !%rbp !%rsp ++
        popq rbp ++ 
        ret ++
        label "print" ++
        pushq !%rbp ++
        pushq !%rdi ++
        movq !%rsp !%rbp ++
        andq (imm (-16)) !%rsp ++
        cmpq (imm 1) (ind rdi) ++
        je "1f" ++
        cmpq (imm 2) (ind rdi) ++
        je "2f" ++
        cmpq (imm 3) (ind rdi) ++
        je "3f" ++
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
        addq (imm 8) !%rdi ++
        movq !%rdi !%rsi ++
        movq (ilab ".Sprint_str") !%rdi ++
        movq (imm 0) !%rax ++
        call "printf" ++
        jmp "7f" ++
        label "7" ++
        popq rax ++
        popq rbp ++
        ret;
      data =
        Hashtbl.fold (fun x _ l -> label x ++ dquad [1] ++ l) genv
          (label ".Sprint_int" ++ string "%d\n" ++
          label ".Sprint_str" ++ string "%s\n" ++
          label ".true" ++ string "true\n" ++
          label ".false" ++ string "false\n")
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
  fprintf fmt "@?";
  close_out f
