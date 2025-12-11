
(* Analyseur lexical pour Petit-Pyret *)

(* TODO : fonctions polymorphes 
-> pour l'instant f < a >(...) passe à l'analyse mais ça sera pas dans les tests trust*)
(* Le lexer ne gère pour l'instant pas les retours à la ligne 
(il les traite comme des blancs seulement)
Ce qui fait que plusieurs instructions peuvent être sur la même ligne*)

{
  open Lexing (*Quoi ?!*)
  open Ast
  open Parser

  exception Lexing_error of string

  let id_or_kwd =
    let h = Hashtbl.create 42 in
    List.iter (fun (s, tok) -> Hashtbl.add h s tok)
      ["lam",LAM ; "if", IF; "else", ELSE;
       "for", FOR;
       "and", BINOP Band; "or", BINOP Bor;
       "true",CBOOL true;
       "false", CBOOL false;
       "block", BLOCK;
       "cases", CASES;
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
  exception Boum
}

let letter = ['a'-'z' 'A'-'Z' '_']
let digit = ['0'-'9']
let ident = letter ('-'* (letter | digit )+)*
let integer = ('-'|'+')? digit+
let space = ' ' | '\t' | '\n'
let blank = space +

rule next_tokens = parse
  (*| '\n'    { [NEWLINE]*)(*On est obligés de produire newline à l'analyse lexicale car le contexte requis pour savoir si la ligne est utile est trop grand*) (*new_line lexbuf*) (*; update_stack (indentation lexbuf)} *) 
  | "#|" { comment lexbuf ; next_token_blank lexbuf} (* Ici le commentaire n'est PAS un blanc. C'est MAAAAAAAAL *) (* Fixed : c'est un blanc avant, mais pas après*)
  | '#' {comment_line lexbuf} 
  | blank {next_token_blank lexbuf}
  | '<' {[LEFT_CHEV]}
  | '>' (blank | '#') {raise(Lexing_error "Illegal blank between '>' and the '(' of type annotations")}
  | '>' {[RIGHT_CHEV]}
  | "->" {[ARROW]}
  (* | ("block"|"else") blank ':'  {raise (Lexing_error("Illegal blank inserted"))} *)
  | "block:" { [BLOCK; COLON]} (* COLON est toujours un lexeme *)
  | "else:" {[ELSE; COLON]}
  | "block" (space|'#') {raise(Lexing_error "Illegal blank after the block keyword")}
  | "else" (space|'#') {raise(Lexing_error "Illegal blank after the else keyword")}
  | '('     { [LP] }
  | ','     { [COMMA] }
  | ':'     { [COLON] }
  | integer as s
            { try [CINT (int_of_string s)]
              with _ -> raise (Lexing_error ("constant too large: " ^ s)) }
  | '\''     { [CSTR (string1 lexbuf)] }
  | '"'     { [CSTR (string2 lexbuf)] }
  | eof     { (*NEWLINE :: unindent 0 @*) [EOF] } 
  | ident as id { [id_or_kwd id] }
  | _ as c  { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }


and next_token_blank = parse 
  | '+'  blank     { [BINOP Badd] }
  | '-'  blank    { [BINOP Bsub] }
  | '*'  blank    { [BINOP Bmul] }
  | "//" blank  { [BINOP Bdiv] }
  | '='  blank   { [EQUAL] }
  | "==" blank   { [BINOP Beq] }
  | "!=" blank   { [BINOP Bneq] }
  | "<"  blank   { [BINOP Blt] }
  | "<=" blank   { [BINOP Ble] }
  | ">"  blank   { [BINOP Bgt] }
  | ">=" blank   { [BINOP Bge] }
  | '(' {raise (Lexing_error("Illegal blank inserted"))} 
  | eof {[EOF]}
  (* On suppose que ( n'est jamais précédé d'un blanc, sauf si l'opérateur précédent doit être suivi d'un blanc *)
  | _ {next_tokens lexbuf}


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
  | _ as c
      { Buffer.add_char string_buffer c;
	string2 lexbuf }

and comment = parse
| "|#" {}
| "#|" {comment lexbuf ; comment lexbuf}
| eof { raise Boum}
| _ {comment lexbuf}

and comment_line = parse 
| '\n' {next_token_blank lexbuf}
| eof {[EOF]}
| _ { comment_line lexbuf}


{

  let next_token =
    let tokens = Queue.create () in (* prochains lexèmes à renvoyer *)
    fun lb ->
      if Queue.is_empty tokens then begin
	let l = next_tokens lb in
	List.iter (fun t -> Queue.add t tokens) l
      end;
      Queue.pop tokens
}


