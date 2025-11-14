
(* Analyseur lexical pour Petit-Pyret *)

{
  open Lexing
  open Ast
  open Parser

  exception Lexing_error of string

  let id_or_kwd =
    let h = Hashtbl.create 32 in
    List.iter (fun (s, tok) -> Hashtbl.add h s tok)
      ["def", DEF; "if", IF; "else", ELSE;
       "return", RETURN; "print", PRINT;
       "for", FOR; "in", IN;
       "and", AND; "or", OR; "not", NOT;
       "True", CST (Cbool true);
       "False", CST (Cbool false);
       "None", CST Cnone;];
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
let integer = (-|+)? digit+
let space = ' ' | '\t' | '\n'
let blank = space +

rule next_tokens = parse
  | '\n'    { new_line lexbuf; update_stack (indentation lexbuf) }
  | "#|" { comment lexbuf ; next_token lexbuf} (* Ici le commentaire n'est PAS un blanc *)
  | ident as id { [id_or_kwd id] }
  | blank '+'  blank     { [PLUS] }
  | blank '-'  blank    { [MINUS] }
  | blank '*'  blank    { [TIMES] }
  | blank "//" blank  { [DIV] }
  | blank '%'  blank   { [MOD] }
  | blank '='  blank   { [EQUAL] }
  | blank "==" blank   { [CMP Beq] }
  | blank "!=" blank   { [CMP Bneq] }
  | blank "<"  blank   { [CMP Blt] }
  | blank "<=" blank   { [CMP Ble] }
  | blank ">"  blank   { [CMP Bgt] }
  | blank ">=" blank   { [CMP Bge] }
  | ')' blank '(' {raise (Lexing_error("Illegal blank inserted"))}
  | ident blank '(' {raise (Lexing_error("Illegal blank inserted"))}
  | ("block"|"else") blank ':'  {raise (Lexing_error("Illegal blank inserted"))}
  | blank {next_token lexbuf}
  | '('     { [LP] }
  | ')'     { [RP] }
  | '['     { [LSQ] }
  | ']'     { [RSQ] }
  | ','     { [COMMA] }
  | ':'     { [COLON] }
  | integer as s
            { try [CST (Cint (int_of_string s))]
              with _ -> raise (Lexing_error ("constant too large: " ^ s)) }
  | '\''     { [CST (Cstring (string1 lexbuf))] }
  | '"'     { [CST (Cstring (string2 lexbuf))] }
  | eof     { NEWLINE :: unindent 0 @ [EOF] } 
  | _ as c  { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }

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


