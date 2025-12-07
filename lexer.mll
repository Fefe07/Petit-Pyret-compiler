
(* Analyseur lexical pour Petit-Pyret *)

(* TODO : fonctions polymorphes *)

{
  open Lexing
  open Ast
  open Parser

  exception Lexing_error of string

  let id_or_kwd =
    let h = Hashtbl.create 42 in
    List.iter (fun (s, tok) -> Hashtbl.add h s tok)
      ["lam",LAM ; "if", IF; "else", ELSE;
       "return", RETURN;
       "for", FOR;
       "and", AND; "or", OR;
       "true",Cbool true;
       "false", Cbool false;
       "block", BLOCK;
       "cases", CASES;
       "end", END;
       "from", FROM ;
       "fun", FUN ;
       "var", VAR 
       ];
   fun s -> try Hashtbl.find h s with Not_found -> IDENT s

  let string_buffer = Buffer.create 1024

  let stack = ref [0]  (* indentation stack *)
  let rec unindent n = match !stack with
    | m :: _ when m = n -> []
    | m :: st when m > n -> stack := st; END :: unindent n
    | _ -> raise (Lexing_error "bad indentation")
  let update_stack n =
    match !stack with
    | m :: _ when m < n ->
      stack := n :: !stack;
      [NEWLINE; BEGIN]
    | _ ->
      NEWLINE :: unindent n
}

let letter = ['a'-'z' 'A'-'Z' '_']
let digit = ['0'-'9']
let ident = letter ('-'* (letter | digit )+)*
let integer = ('-'|'+')? digit+
let space = ' ' | '\t' | '\n'
let blank = space +

rule next_tokens = parse
  | '\n'    { new_line lexbuf; update_stack (indentation lexbuf) }
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
            { try [CST (Cint (int_of_string s))]
              with _ -> raise (Lexing_error ("constant too large: " ^ s)) }
  | '\''     { [CST (Cstring (string1 lexbuf))] }
  | '"'     { [CST (Cstring (string2 lexbuf))] }
  | eof     { NEWLINE :: unindent 0 @ [EOF] } 
  | ident as id { [id_or_kwd id] }
  | _ as c  { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }


and next_token_blank = parse 
  | '+'  blank     { [PLUS] }
  | '-'  blank    { [MINUS] }
  | '*'  blank    { [TIMES] }
  | "//" blank  { [DIV] }
  | '%'  blank   { [MOD] }
  | '='  blank   { [EQUAL] }
  | "==" blank   { [CMP Beq] }
  | "!=" blank   { [CMP Bneq] }
  | "<"  blank   { [CMP Blt] }
  | "<=" blank   { [CMP Ble] }
  | ">"  blank   { [CMP Bgt] }
  | ">=" blank   { [CMP Bge] }
  | '(' {raise (Lexing_error("Illegal blank inserted"))} 
  (* On suppose que ( n'est jamais précédé d'un blanc, sauf si l'opérateur précédent doit être suivi d'un blanc *)
  | _ {next_tokens lexbuf}


and indentation = parse
  | (space | comment)* '\n'
      { new_line lexbuf; indentation lexbuf }
  | space* as s
      { String.length s }
  | space* eof
      { 0 }


and string1 = parse
  | '\'' 
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
      { Bufer.add_char string_buffer '\\' ; string lexbuf}
  | '\n' {raise(Lexing_error ("Newlines forbidden in strings, use \\n instead"))}
  | _ as c
      { Buffer.add_char string_buffer c;
	string lexbuf }
  | eof
      { raise (Lexing_error "unterminated string") }

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
      { Bufer.add_char string_buffer '\\' ; string lexbuf}
  | '\n' {raise(Lexing_error ("Newlines forbidden in strings, use \\n instead"))}
  | _ as c
      { Buffer.add_char string_buffer c;
	string lexbuf }
  | eof
      { raise (Lexing_error "unterminated string") }

and comment = parse
| "|#" {}
| "#|" {comment lexbuf ; comment lexbuf}
| _ -> {comment lexbuf}

and comment_line = parse 
| '\n' {next_token_blank lexbuf}
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


