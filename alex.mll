(* The lexer definition *)

{
     open Util;;
(**) open Asyn;;


(* The table of keywords *)

let keyword_table = (Hashtbl.create 100 : (string, token) Hashtbl.t)
;;

do_list (fun (str,tok) -> Hashtbl.add keyword_table str tok) [
  "else", ELSE;
  "function", FUNCTION;
  "if", IF;
  "in", IN;
  "let", LET;
  "rec", REC;
  "ref", REF;
  "then", THEN
];;

let add_infix s =
  Hashtbl.add keyword_table s (INFIX s)
;;

let remove_infix s =
  Hashtbl.remove keyword_table s
;;


(* To buffer string literals *)

let initial_string_buffer = create_string 256;;
let string_buff = ref initial_string_buffer;;
let string_index = ref 0;;

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0;
  ()
;;

let store_string_char c =
  if !string_index >= string_length (!string_buff) then begin
    let new_buff = create_string (string_length (!string_buff) * 2) in
      blit_string (!string_buff) 0 new_buff 0 (string_length (!string_buff));
      string_buff := new_buff
  end;
  set_nth_char (!string_buff) (!string_index) c;
  incr string_index
;;

let get_stored_string () =
  let s = sub_string (!string_buff) 0 (!string_index) in
    string_buff := initial_string_buffer;
    s
;;

(* To translate escape sequences *)

let char_for_backslash = function

    'n' -> '\010'
  | 'r' -> '\013'

  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c
;;

let char_for_decimal_code lexbuf i =
  let c = 
    100 * (int_of_char(get_lexeme_char lexbuf i) - 48) +
     10 * (int_of_char(get_lexeme_char lexbuf (i+1)) - 48) +
          (int_of_char(get_lexeme_char lexbuf (i+2)) - 48) in
  char_of_int(c land 0xFF)
;;

}






rule main = parse
    [' ' '\010' '\013' '\009' '\012'] +
      { main lexbuf }
  | ['A'-'Z' 'a'-'z'  ] ( '_' ? ['A'-'Z' 'a'-'z'  '0'-'9' ] ) *
      { let s = get_lexeme lexbuf in
          try
            Hashtbl.find keyword_table s
          with Not_found ->
             IDENT s }
  | ['0'-'9']+
    | '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+
    | '0' ['o' 'O'] ['0'-'7']+
    | '0' ['b' 'B'] ['0'-'1']+
      { INT (int_of_string(get_lexeme lexbuf)) }
  | ['0'-'9']+ ('.' ['0'-'9']*)? (['e' 'E'] ['+' '-']? ['0'-'9']+)?
      { FLOAT (float_of_string(get_lexeme lexbuf)) }
  | "\""
      { reset_string_buffer();
        let string_start = lexbuf.Lexing.lex_start_pos + lexbuf.Lexing.lex_abs_pos in
        begin try
          string lexbuf
        with Error(Unterminated_string, _, string_end) ->
          raise(Error(Unterminated_string, string_start, string_end))
        end;
        lexbuf.Lexing.lex_start_pos <- string_start - lexbuf.Lexing.lex_abs_pos;
        STRING (get_stored_string()) }


  | "(" { LPAREN }
  | ")" { RPAREN }
  | "," { COMMA }
  | "->" { MINUSGREATER }
  | "::" { COLONCOLON }
  | ":=" { COLONEQUAL }
  | ";" { SEMI }
  | ";;" { SEMISEMI }
  | "=" { EQUAL }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
 

  | [ '!' '?' ] [ '!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~'  ] *
            { PREFIX(get_lexeme lexbuf) }

  | [ '=' '<' '>' '@' '^' '|' '&' '~' '$' ] [ '!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~'  ] *
            { INFIX(get_lexeme lexbuf) }
  | [ '+' '-' ] [ '!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~'  ] *
            { INFIX(get_lexeme lexbuf) }
  | "**" [ '!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~'  ] *
            { INFIX(get_lexeme lexbuf) }
  | [ '*' '/' '%' ] [ '!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~'  ] *
            { INFIX(get_lexeme lexbuf) }
  | eof  { EOF }
  | _
      { raise (Error(Illegal_character,
                            Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)) }


and string = parse
    '"' (*"'"'*)
      { () }
  | '\\' ("\010" | "\013" | "\013\010") [' ' '\009'] *
      { string lexbuf }
  | '\\' ['\\' '"' (*"'"'*) 'n' 't' 'b' 'r']
      { store_string_char(char_for_backslash(get_lexeme_char lexbuf 1));
        string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { store_string_char(char_for_decimal_code lexbuf 1);
         string lexbuf }
  | eof 
      { raise (Error
                (Unterminated_string, 0, Lexing.lexeme_start lexbuf)) }
  | _
      { store_string_char(get_lexeme_char lexbuf 0);
        string lexbuf }









