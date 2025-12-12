
(* Analyseur lexical pour Petit-Pyret *)


{
  open Lexing 
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

  | _ -> print_string " pas traité\n" 

  ) ; flush stdout

  let blank_before = ref false
  let num_before = ref false
}

let letter = ['a'-'z' 'A'-'Z' '_']
let digit = ['0'-'9']
let ident = letter ('-'* (letter | digit )+)*
let integer = ('+'|'-')? digit+
let space = ' ' | '\t' | '\n'
let blank = space +

rule next_tokens = parse
  | "#|" { comment lexbuf; blank_before := true; next_tokens lexbuf }
  | '#' { comment_line lexbuf } 
  | (' '|'\t')* { blank_before := true; next_tokens lexbuf }
  | '\n' {
      blank_before := true; num_before := false; next_tokens lexbuf
    }
  | '+' blank {
      if not !blank_before
      then raise (Lexing_error "Missing blank before +");
      num_before := false; [PLUS]
    }
  | '-' blank {
      if not !blank_before
      then raise (Lexing_error "Missing blank before -") ;
      num_before := false; [MINUS]
    }
  | '*' blank {
      if not !blank_before
      then raise (Lexing_error "Missing blank before *") ;
      num_before := false; [MUL]
    }
  | "/" blank {
      if not !blank_before
      then raise (Lexing_error "Missing blank before //") ;
      num_before := false; [DIV]
    }
  | "==" blank {
      if not !blank_before
      then raise (Lexing_error "Missing blank before ==") ;
      num_before := false; [BINOP Beq]
    }
  | "<>" blank {
      if not !blank_before
      then raise (Lexing_error "Missing blank before !=") ;
      num_before := false; [BINOP Bneq]
    }
  | "<" blank { 
      if not !blank_before
      then (blank_before := true ;num_before := false;  [LEFT_CHEV])
      else (num_before := false; [LT])
    }
  | "<=" blank   {
      if not !blank_before
      then raise (Lexing_error "Missing blank before <=");
      num_before := false; [BINOP Ble]
    }
  | ">" blank { 
      if not !blank_before
      then (blank_before := true; num_before := false; [RIGHT_CHEV])
      else (num_before := false; [GT] )
    }
  | ">=" blank {
      if not !blank_before
      then raise (Lexing_error "Missing blank before >=");
      num_before := false; [BINOP Bge]
    }
  | '=' { blank_before := false; num_before := false; [EQUAL] }
  | "=>" { blank_before := false; num_before := false; [DOUBLEARROW] }
  | '<' { blank_before := false; num_before := false; [LEFT_CHEV] }
  | ">(" {
      blank_before := false; num_before := false;
      [RIGHT_CHEV; LP_CALL]
    }
  | '>' blank '(' {
      raise(Lexing_error
      "Illegal blank or text between '>' and the '('")
    }
  | '>' { blank_before := false; num_before := false; [RIGHT_CHEV] }
  | "->" { blank_before := false; num_before := false; [ARROW] }
  | "block:" {
      blank_before := false; num_before := false; [BLOCK; COLON]
    }
  | "else:" {
      blank_before := false ;num_before := false; [ELSE; COLON]
    }
  | "else if" {
      blank_before := false ;num_before := false; [ELSE; IF]
    }
  | "block" (space|'#') {
      raise (Lexing_error "Illegal blank after the block keyword")
    }
  | "else" (space|'#') {
      raise (Lexing_error "Illegal blank after the else keyword")
   }
  | "cases" (space|'\t')* '(' { num_before := false; [CASES; LP] }
  | "cases" { num_before := false; [CASES] }
  | '(' { blank_before := false; num_before := false; [LP] }
  | ")(" {
      blank_before := false; num_before := false; [RP; LP_CALL]
    }
  | ')'  { blank_before := false; num_before := false; [RP] }
  | ','  { blank_before := false; num_before := false; [COMMA] }
  | "::" { blank_before := false; num_before := false; [DBLCOLON] }
  | ":=" { blank_before := false; num_before := false; [DEF] }
  | ':'  { blank_before := false ;num_before := false; [COLON]}
  | '|'  { blank_before := false ;num_before := false; [PIPE]}
  | integer as s {
      if !num_before
      then raise (Lexing_error "need operator between integers")
      else try
        blank_before := false; num_before := true;
        [CINT (int_of_string s)]
      with _ -> raise (Lexing_error ("constant too large: " ^ s))
    }
  | '\'' {
      blank_before := false; num_before := false;
      [CSTR (string1 lexbuf)]
    }
  | '"' {
      blank_before := false; num_before := false;
      [CSTR (string2 lexbuf)]
    }
  | eof { [EOF] } 
  | ident as id; '(' {
      blank_before := false;
      [id_or_kwd id; LP_CALL]
    }
  | ident as id {
      blank_before := false;
      let p = id_or_kwd id in 
      match p with 
      | IDENT s -> num_before:=true; [p]
      | _ -> num_before := false; [p]
    }
  | ident (space|'\t')+ '(' {
      raise (Lexing_error
      "Illegal blank between caller and arguments")
    }
  | _ as c  {
      raise (Lexing_error ("illegal character: " ^ String.make 1 c))
    }

(* string1 is delimited with '' and string2 with ""  *)
and string1 = parse
  | '\'' {
      let s = Buffer.contents string_buffer in
	    Buffer.reset string_buffer;
      s
    }
  | "\\n" {
      Buffer.add_char string_buffer '\n';
	    string1 lexbuf
    }
  | "\\t" {
      Buffer.add_char string_buffer '\t';
	    string1 lexbuf
    }
  | "\\\"" {
      Buffer.add_char string_buffer '"';
	    string1 lexbuf
    }
  | "\\\'" {
      Buffer.add_char string_buffer '\'';
	    string1 lexbuf
    }
  | "\\\\" { Buffer.add_char string_buffer '\\'; string1 lexbuf }
  | '\n' {
      raise(Lexing_error 
      "Newlines forbidden in strings, use \\n instead")
    }
  | eof { raise (Lexing_error "unterminated string") }
  | '\\' { raise (Lexing_error "undefined escape sequence") }
  | _ as c {
      Buffer.add_char string_buffer c;
	    string1 lexbuf
    }


and string2 = parse
  | '"' {
    let s = Buffer.contents string_buffer in
	  Buffer.reset string_buffer;
	  s
    }
  | "\\n"  { Buffer.add_char string_buffer '\n'; string2 lexbuf }
  | "\\t"  { Buffer.add_char string_buffer '\t'; string2 lexbuf }
  | "\\\"" { Buffer.add_char string_buffer '"'; string2 lexbuf }
  | "\\\'" { Buffer.add_char string_buffer '\''; string2 lexbuf }
  | "\\\\" { Buffer.add_char string_buffer '\\'; string2 lexbuf }
  | '\n'   {
      raise (Lexing_error 
      "Newlines forbidden in strings, use \\n instead")
    }

  | eof  { raise (Lexing_error "unterminated string") }
  | '\\' { raise (Lexing_error "undefined escape sequence") }
  | _ as c { Buffer.add_char string_buffer c; string2 lexbuf }

and comment = parse
| "|#" { }
| "#|" { comment lexbuf; comment lexbuf }
| eof  { raise (Lexing_error "Commentaire non fermé") }
| _ { comment lexbuf }

and comment_line = parse 
| '\n' { blank_before := true; next_tokens lexbuf }
| eof  { [EOF] }
| _    { comment_line lexbuf }


{
  let next_token =
    let tokens = Queue.create () in (* prochains lexèmes à renvoyer *)
    fun lb ->
      blank_before := false ;
      if Queue.is_empty tokens then begin
	      let l = next_tokens lb in
	      List.iter (fun t -> Queue.add t tokens) l
      end;
      Queue.pop tokens
}


