
(* Analyseur lexical pour Petit-Pyret *)

(* TODO : fonctions polymorphes 
-> pour l'instant f < a >(...) passe à l'analyse mais 
ça sera pas dans les tests trust*)
(* Zut c'est dans les tests fun-4/5 *)
(* Même problème pour List < Any >, testé dans testfile-type-1/2 *)
(* Le lexing actuel ne permet pas de distinguer un chevron et un opérateur  
Solutions : 
- Lexer avec plus de contexte ? 
- Même Lexeme (un peu moins élégant mais plus efficace je pense *)

(* Le lexer ne gère pour l'instant pas les retours à la ligne 
(il les traite comme des blancs seulement)
Ce qui fait que plusieurs instructions peuvent être sur la même ligne.
Résultat : 
- print (1) est accepté car il considère que c'est plusieurs statements 
- 1+2 et 1-2 aussi 
Solution choisie : règler ces deux problèmes à la main 
(conseil de Samuel) *)

{
  open Lexing (*Quoi ?!*)
  open Ast
  open Parser

  exception Lexing_error of string

  let id_or_kwd =
    let h = Hashtbl.create 42 in
    List.iter (fun (s, tok) -> Hashtbl.add h s tok)
      ["lam",LAM ; "if", IF;
       "for", FOR;
       "and", BINOP Band; "or", BINOP Bor;
       "true",CBOOL true;
       "false", CBOOL false;
       "block", BLOCK;
       "end", END;
       "from", FROM ;
       "fun", FUN ;
       "var", VAR 
       ];
   fun s -> try Hashtbl.find h s with Not_found -> IDENT s

  let string_buffer = Buffer.create 1024


  (*A modifier !!!!! Pas besoin de produire ces lexèmes*)
  (*Pas besoin d'indentation au fait *)
  (* let stack = ref [0]  (* indentation stack *)
  let rec unindent n = match !stack with
    | m :: _ when m = n -> []
    | m :: st when m > n -> stack := st; END :: unindent n
    | _ -> raise (Lexing_error "bad indentation")
  let update_stack n =
    match !stack with
    | m :: _ when m < n ->
      stack := n :: !stack;
      (*[NEWLINE; BEGIN]*)[]
    | _ ->
      NEWLINE :: unindent n *)

  (* Ugly printer *)
  let pp_tok t = 
  (match t with 
  | BINOP Blt -> print_string "<\n"
  | BINOP Bgt -> print_string ">\n"
  | EQUAL -> print_string "=\n"
  | IDENT s -> print_string (s^"\n") 
  | FUN  -> print_string "fun\n"
  | LEFT_CHEV -> print_string "left_chev\n" 
  | CINT n -> print_int n 
  | BINOP Badd -> print_string "+\n"
  | EOF -> print_string "eof\n"

  | _ -> print_string "pas traité\n" 

  ) ; flush stdout

  let blank_before = ref false
}

let letter = ['a'-'z' 'A'-'Z' '_']
let digit = ['0'-'9']
let ident = letter ('-'* (letter | digit )+)*
let integer = ('+'|'-')? digit+
let space = ' ' | '\t' | '\n'
let blank = space +

rule next_tokens = parse
  (*| '\n'    { [NEWLINE]*)(*On est obligés de produire newline à l'analyse lexicale car le contexte requis pour savoir si la ligne est utile est trop grand*) (*new_line lexbuf*) (*; update_stack (indentation lexbuf)} *) 
  | "#|" { comment lexbuf ; blank_before := true ; next_tokens lexbuf} (* Ici le commentaire n'est PAS un blanc. C'est MAAAAAAAAL *) (* Fixed : c'est un blanc avant, mais pas après*)
  | '#' {comment_line lexbuf} 
  | blank {blank_before := true ; next_tokens lexbuf}
  | '+'  blank     { if not !blank_before then 
    raise (Lexing_error "Missing blank before +") ;
    [PLUS] }
  | '-'  blank    { if not !blank_before then 
    raise (Lexing_error "Missing blank before -") ;
    [MINUS] }
  | '*'  blank    { if not !blank_before then 
    raise (Lexing_error "Missing blank before *") ;
    [MUL] }
  | "/" blank  { if not !blank_before then 
    raise (Lexing_error "Missing blank before /") ;
    [DIV] }

  | "==" blank   { if not !blank_before then 
    raise (Lexing_error "Missing blank before ==") ;
    [BINOP Beq] }
  | "<>" blank   { if not !blank_before then 
    raise (Lexing_error "Missing blank before <>") ;
    [BINOP Bneq] }
  | "<"  blank   { 
    if not !blank_before then 
    (blank_before := true ; [LEFT_CHEV]) else
    (*[BINOP Blt]*)[LT] }
  | "<=" blank   { if not !blank_before then 
    raise (Lexing_error "Missing blank before <=") ;
    [BINOP Ble] }
  | ">"  blank   { 
    if not !blank_before then 
    (blank_before := true  ;[RIGHT_CHEV])
    else
    (*[BINOP Bgt]*)[GT] }
  | ">=" blank   {if not !blank_before then 
    raise (Lexing_error "Missing blank before >=") ;
    [BINOP Bge] }
  | '=' { blank_before := false ; [EQUAL]}
  | "=>" { blank_before := false ; [DOUBLEARROW]}
  | '<' { blank_before := false ; [LEFT_CHEV]}
  | ">(" { blank_before := false ;[RIGHT_CHEV; LP_CALL]}
  | '>' blank '(' {raise(Lexing_error "Illegal blank or text between '>' and the '(' of type annotations")}
  | '>' { blank_before := false ; [RIGHT_CHEV]}
  | "->" { blank_before := false ;[ARROW]}
  (* | ("block"|"else") blank ':'  {raise (Lexing_error("Illegal blank inserted"))} *)
  | "block:" { blank_before := false ; [BLOCK; COLON]} (* COLON est toujours un lexeme *)
  | "else:" {blank_before := false ;[ELSE; COLON]}
  | "else if" {blank_before := false ;[ELSE; IF]}
  | "block" (space|'#') {raise(Lexing_error "Illegal blank after the block keyword")}
  | "else" (space|'#') {raise(Lexing_error "Illegal blank after the else keyword")}
  | "cases" (space|'\t')* '(' {[CASES;LP]}
  | "cases"  {[CASES]}
  | '('     { blank_before := false ; [LP] }
  | ")("    {blank_before := false ; [RP ; LP_CALL]}
  | ')'     { blank_before := false ;[RP] }
  | ','     { blank_before := false ;[COMMA] }
  | "::"    { blank_before := false ;[DBLCOLON] }
  | ":="     { blank_before := false ;[DEF] }
  | ':'     { blank_before := false ;[COLON]}
  | '|'     { blank_before := false ;[PIPE]}
  | integer as s
            { try blank_before := false ; [CINT (int_of_string s)]
              with _ -> raise (Lexing_error ("constant too large: " ^ s)) }
  | '\''     { blank_before := false ;[CSTR (string1 lexbuf)] }
  | '"'     { blank_before := false ;[CSTR (string2 lexbuf)] }
  | eof     { (*NEWLINE :: unindent 0 @*) [EOF] } 
  | ident as id '(' { (*Printf.printf "%s\n" id ;*) blank_before := false ;[id_or_kwd id ; LP_CALL] }
  | ident as id {  blank_before := false ;[id_or_kwd id] }
  | ident as id (space|'\t')+ '(' {raise (Lexing_error "Illegal blank between caller and arguments")}
  | _ as c  { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }

(* Commentaires mal gérés après les opérateurs *)
(* and next_token_blank = parse 
  (* | blank {next_token_blank lexbuf} *)

  (*| '(' {raise (Lexing_error("Illegal blank inserted"))} *)(*Pas besoin, génerera une erreur de syntaxe*)
    (* On suppose que ( n'est jamais précédé d'un blanc, sauf si l'opérateur précédent doit être suivi d'un blanc *)
  | eof {[EOF]}
  (*|  {next_tokens lexbuf} *)
  (* AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAH Ce truc me bouffe le premier caractère!!!!!!!!!!!!!!!!!!!*)
  | "#|" { comment lexbuf ; next_token_blank lexbuf} (* Ici le commentaire n'est PAS un blanc. C'est MAAAAAAAAL *) (* Fixed : c'est un blanc avant, mais pas après*)
  | '#' {comment_line lexbuf} 
  | blank {next_token_blank lexbuf}
  | '<' { [LEFT_CHEV]}
  | ">(" {[RIGHT_CHEV; LP_CALL]}
  | '>' {raise(Lexing_error "Illegal blank between '>' and the '(' of type annotations")}
  | "->" {[ARROW]}
  (* | ("block"|"else") blank ':'  {raise (Lexing_error("Illegal blank inserted"))} *)
  | "block:" { [BLOCK; COLON]} (* COLON est toujours un lexeme *)
  (* | "else:" {[ELSE; COLON]} *)
  | "block" (space|'#') {raise(Lexing_error "Illegal blank after the block keyword")}
  (* | "else" (space|'#') {raise(Lexing_error "Illegal blank after the else keyword")} *)
  | '('     { [LP] }
  | ")("    { [RP ; LP_CALL]}
  | ')'     { [RP] }
  | ','     { [COMMA] }
  | ':'     { [COLON] }
  | integer as s
            { try [CINT (int_of_string s)]
              with _ -> raise (Lexing_error ("constant too large: " ^ s)) }
  | '\''     { [CSTR (string1 lexbuf)] }
  | '"'     { [CSTR (string2 lexbuf)] }
  | eof     { (*NEWLINE :: unindent 0 @*) [EOF] } 
  | ident as id '(' { (*Printf.printf "%s\n" id ;*) [id_or_kwd id ; LP_CALL] }
  | ident as id {  [id_or_kwd id] }
  | _ as c  { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) } *)

(* and indentation = parse
  | (space(* | comment*))* '\n'
      { new_line lexbuf; indentation lexbuf }
  | space* as s
      { String.length s }
  | space* eof
      { 0 } *)

(* string1 is delimited with '' and string2 with ""  *)
and string1 = parse
  | '\'' 
      { let s = Buffer.contents string_buffer in
	Buffer.reset string_buffer;
	s }
  | "\\n"
      { Buffer.add_char string_buffer '\n';
	string1 lexbuf }
  | "\\t"
      { Buffer.add_char string_buffer '\t';
	string1 lexbuf }
  | "\\\""
      { Buffer.add_char string_buffer '"';
	string1 lexbuf }
  | "\\\'"
      { Buffer.add_char string_buffer '\'';
	string1 lexbuf }
  | "\\\\" 
      { Buffer.add_char string_buffer '\\' ; string1 lexbuf}
  | '\n' {raise(Lexing_error ("Newlines forbidden in strings, use \\n instead"))}
  | eof
    { raise (Lexing_error "unterminated string") }
  | '\\' {raise (Lexing_error "undefined escape sequence")}
  | _ as c
      { Buffer.add_char string_buffer c;
	string1 lexbuf }


and string2 = parse
  | '"' 
      { let s = Buffer.contents string_buffer in
	Buffer.reset string_buffer;
	s }
  | "\\n"
      { Buffer.add_char string_buffer '\n';
	string2 lexbuf }
  | "\\t"
      { Buffer.add_char string_buffer '\t';
	string2 lexbuf }
  | "\\\""
      { Buffer.add_char string_buffer '"';
	string2 lexbuf }
  | "\\\'"
      { Buffer.add_char string_buffer '\'';
	string2 lexbuf }
  | "\\\\" 
      { Buffer.add_char string_buffer '\\' ; string2 lexbuf}
  | '\n' {raise(Lexing_error ("Newlines forbidden in strings, use \\n instead"))}

  | eof
      { raise (Lexing_error "unterminated string") }
  | '\\' {raise (Lexing_error "undefined escape sequence")}
  | _ as c
      { Buffer.add_char string_buffer c;
	string2 lexbuf }

and comment = parse
| "|#" {}
| "#|" {comment lexbuf ; comment lexbuf}
| eof { raise (Lexing_error "Commentaire pas fermé")}
| _ {comment lexbuf}

and comment_line = parse 
| '\n' {blank_before := true ; next_tokens lexbuf}
| eof {[EOF]}
| _ { comment_line lexbuf}


{

  let next_token =
    let tokens = Queue.create () in (* prochains lexèmes à renvoyer *)
    fun lb ->
      blank_before := false ;if Queue.is_empty tokens then begin
	let l = next_tokens lb in
	List.iter (fun t -> Queue.add t tokens) l
      end;
      Queue.pop tokens
}


