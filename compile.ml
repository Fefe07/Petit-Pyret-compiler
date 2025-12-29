
open Format
open X86_64
open Ast

(* phase 1 : allocation des variables *)

exception VarUndef of string

let (genv : (string, unit) Hashtbl.t) = Hashtbl.create 17

module Smap = Map.Make(String)

type local_env = ident Smap.t

let rec alloc_expr (env: local_env) (fpcur: int) = function
  | PCst i ->
    Cst i, fpcur

  | PVar x -> (match Smap.find_opt x env with
    | Some i -> LVar i, fpcur
    | None -> if not (Hashtbl.mem genv x) then raise (VarUndef x)
        else GVar x, fpcur)

  | PBinop (o, e1, e2)->
    let exp1, s1 = alloc_expr env fpcur e1 in
    let exp2, s2 = alloc_expr env fpcur e2 in
    Binop (o, exp1, exp2), max s1 s2

  | PLetin (x, e1, e2) ->
    let exp1, s1 = alloc_expr env fpcur e1 in 
    let exp2, s2 = alloc_expr (Smap.add x (-(fpcur + 8)) env) (fpcur+8) e2 in 
    Letin (-(fpcur + 8), exp1, exp2), max s1 s2

  | PCall (f, l) ->
    let l, s = List.fold_right (fun e (le, s) -> 
      let e', s' = alloc_expr env fpcur e in (e'::le, max s s'))
      l ([],fpcur) in Call (f,l) , s


let alloc_stmt = function
  | PSet (x, e) ->
    let e, s = alloc_expr Smap.empty 0 e in
    (Hashtbl.add genv x () ; Set (x, e, s))

  | PFun (f, l, e) ->
    let new_env, _ = List.fold_left (fun (env,i) s -> (Smap.add s (8*i) env, i+1))
    (Smap.empty, 2) l in 
    let e', s' = alloc_expr new_env 0 e in Fun (f, e', s')

  | PPrint e ->
    let e, fpmax = alloc_expr Smap.empty 0 e in
    Print (e, fpmax)

let alloc = List.map alloc_stmt

(******************************************************************************)
(* phase 2 : production de code *)

let popn n = addq (imm n) !%rsp
let pushn n = subq (imm n) !%rsp

let rec compile_expr = function
  | Cst i ->
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

let compile_program p ofile =
  let p = alloc p in
  Format.eprintf "%a@." print p;
  let codefun, code = List.fold_left compile_stmt (nop, nop) p in
  let p =
    { text =
        globl "main" ++ label "main" ++
        movq !%rsp !%rbp ++
        code ++
        movq (imm 0) !%rax ++ (* exit *)
        ret ++
        label "print_int" ++
        movq !%rdi !%rsi ++
        movq (ilab ".Sprint_int") !%rdi ++
        movq (imm 0) !%rax ++
        call "printf" ++
        ret ++
        codefun;
      data =
        Hashtbl.fold (fun x _ l -> label x ++ dquad [1] ++ l) genv
          (label ".Sprint_int" ++ string "%d\n")
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
  fprintf fmt "@?";
  close_out f
