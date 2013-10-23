{

open Mp8common;;

let line_count = ref 1
let char_count = ref 1

let cinc n = char_count := !char_count + n
let linc n = line_count := (char_count := 1; !line_count + n)

}

(* You can assign names to commonly-used regular expressions in this part
   of the code, to save the trouble of re-typing them each time they are used *)

let numeric = ['0' - '9']
let lowercase = ['a' - 'z']
let uppercase = ['A' - 'Z']
let character = ['a' - 'z' 'A' - 'Z']
let idchar = numeric | character | '_' | '''
let letter =['a' - 'z' 'A' - 'Z' '_']
let open_comment = "(*"
let close_comment = "*)"
let legal_char = [^ '"']

rule token = parse
  | [' ' '\t' '\n'] { token lexbuf }  (* skip over whitespace *)
  | eof             { EOF } 
  | '~'             { NEG }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { TIMES }
  | '/'             { DIV }
  | "+."            { DPLUS }
  | "-."            { DMINUS }
  | "*."            { DTIMES }
  | "/."            { DDIV }
  | '^'             { CARAT }
  | '<'             { LT }
  | '>'             { GT }
  | "<="            { LEQ }
  | ">="            { GEQ }
  | '='             { EQUALS }
  | "<>"            { NEQ }
  | '|'             { PIPE }
  | "=>"            { ARROW }
  | ';'             { SEMI }
  | "::"            { DCOLON }
  | '@'             { AT }
  | "nil"           { NIL }
  | "let"           { LET }
  | "local"         { LOCAL }
  | "val"           { VAL }
  | "rec"           { REC }
  | "and"           { AND }
  | "end"           { END }
  | "in"            { IN }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
  | "fun"           { FUN }
  | "fn"            { FN }
  | "op"            { OP }
  | "mod"           { MOD }
  | "raise"         { RAISE }
  | "handle"        { HANDLE }
  | "with"          { WITH }
  | "not"           { NOT }
  | "andalso"       { ANDALSO }
  | "orelse"        { ORELSE }
  | '['             { LBRAC }
  | ']'             { RBRAC }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | ','             { COMMA }
  | '_'             { UNDERSCORE }
  | numeric+ as num                  { INT(int_of_string num) }
  | numeric+'.'numeric+ as num       { REAL(float_of_string num) }
  | "true"          { BOOL(true) }
  | "false"         { BOOL(false) }
  | "()"            { UNIT }
  | (character)(idchar)* as id       { IDENT(id) }
  | ";;" [^'\n']*   { token lexbuf }
  | open_comment    { comment 1 lexbuf }
  | close_comment   { raise (Failure "unmatched comment") }
  | "\""            { string "" lexbuf }

and comment depth = parse
  | open_comment    { comment (depth+1) lexbuf }
  | close_comment   { if depth = 1 then token lexbuf else comment (depth-1) lexbuf }
  | eof             { raise (Failure "unmatched comment") }
  | _               { comment depth lexbuf }

and string str = parse
  | "\""            { STRING(str) }
  | "\\"            { escape str lexbuf }
  | _ as c          { string (str ^ (String.make 1 c)) lexbuf }

and escape str = parse
  | "\\"            { string (str ^ (String.make 1 '\\')) lexbuf }
  | "\""            { string (str ^ (String.make 1 '\"')) lexbuf }
  | "\\'"           { print_string "here"; string (str ^ (String.make 1 '\'')) lexbuf }
  | "t"             { string (str ^ (String.make 1 '\t')) lexbuf }
  | "n"             { string (str ^ (String.make 1 '\n')) lexbuf }
  | "r"             { string (str ^ (String.make 1 '\r')) lexbuf }
  | (numeric)(numeric)(numeric) as num      { let n = int_of_string(num) in if (n > 255)
    then string (str ^ "\\" ^ num) lexbuf
    else string (str ^ (String.make 1 (char_of_int (int_of_string num)))) lexbuf }
  | _ as c          { string (str ^ (String.make 1 '\\') ^ (String.make 1  c)) lexbuf }

{(* do not modify this function: *)
 let lextest s = token (Lexing.from_string s)

let opcom r = OPCOM(r.line_num,r.char_num)
let clcom r = CLCOM(r.line_num,r.char_num)
let sclcom r = SCLCOM(r.line_num,r.char_num)

  let get_all_tokens s =
      let _ = char_count := 1 in
      let _ = line_count := 1 in
      let b = Lexing.from_string (s^"\n") in
      let rec g () = 
      match token b with EOF -> []
      | t -> t :: g () in
      g ()

let try_get_all_tokens s =
    try Some (get_all_tokens s) with Failure "unmatched comment" -> None
    	     			      	 | OpenComm r -> None
    	     			      	 | CloseComm r -> None
    	     			      	 | SuperCloseComm r -> None
let try_comm_get_all_tokens s =
    try Some (get_all_tokens s) with Failure "unmatched comment" -> None
    	     			      	 | OpenComm r -> Some ([opcom r])
    	     			      	 | CloseComm r -> Some ([clcom r])
    	     			      	 | SuperCloseComm r -> Some ([sclcom r])

 }

