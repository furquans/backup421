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
  | [' ' '\t']      { cinc 1; token lexbuf }  (* skip over whitespace *)
  | ['\n']          { linc 1; token lexbuf }
  | eof             { EOF } 
  | '~'             { cinc 1; NEG }
  | '+'             { cinc 1; PLUS }
  | '-'             { cinc 1; MINUS }
  | '*'             { cinc 1; TIMES }
  | '/'             { cinc 1; DIV }
  | "+."            { cinc 2; DPLUS }
  | "-."            { cinc 2; DMINUS }
  | "*."            { cinc 2; DTIMES }
  | "/."            { cinc 2; DDIV }
  | '^'             { cinc 1; CARAT }
  | '<'             { cinc 1; LT }
  | '>'             { cinc 1; GT }
  | "<="            { cinc 2; LEQ }
  | ">="            { cinc 2; GEQ }
  | '='             { cinc 1; EQUALS }
  | "<>"            { cinc 2; NEQ }
  | '|'             { cinc 1; PIPE }
  | "=>"            { cinc 2; ARROW }
  | ';'             { cinc 1; SEMI }
  | "::"            { cinc 2; DCOLON }
  | '@'             { cinc 1; AT }
  | "nil"           { cinc 3; NIL }
  | "let"           { cinc 3; LET }
  | "local"         { cinc 5; LOCAL }
  | "val"           { cinc 3; VAL }
  | "rec"           { cinc 3; REC }
  | "and"           { cinc 3; AND }
  | "end"           { cinc 3; END }
  | "in"            { cinc 2; IN }
  | "if"            { cinc 2; IF }
  | "then"          { cinc 4; THEN }
  | "else"          { cinc 4; ELSE }
  | "fun"           { cinc 3; FUN }
  | "fn"            { cinc 2; FN }
  | "op"            { cinc 2; OP }
  | "mod"           { cinc 3; MOD }
  | "raise"         { cinc 5; RAISE }
  | "handle"        { cinc 6; HANDLE }
  | "with"          { cinc 4; WITH }
  | "not"           { cinc 3; NOT }
  | "andalso"       { cinc 7; ANDALSO }
  | "orelse"        { cinc 6; ORELSE }
  | '['             { cinc 1; LBRAC }
  | ']'             { cinc 1; RBRAC }
  | '('             { cinc 1; LPAREN }
  | ')'             { cinc 1; RPAREN }
  | ','             { cinc 1; COMMA }
  | '_'             { cinc 1; UNDERSCORE }
  | numeric+ as num                  { cinc (String.length num); INT(int_of_string num) }
  | numeric+'.'numeric+ as num       { cinc (String.length num); REAL(float_of_string num) }
  | "true"          { cinc 4; BOOL(true) }
  | "false"         { cinc 5; BOOL(false) }
  | "()"            { cinc 2; UNIT }
  | (character)(idchar)* as id       { cinc (String.length id); IDENT(id) }
  | ";;" [^'\n']*   { linc 1; token lexbuf }
  | open_comment    { cinc 2; comment 1 [(!line_count,!char_count-2)] lexbuf }
  | close_comment   { raise (CloseComm{line_num = !line_count; char_num = !char_count}) }
  | "\""            { cinc 1; string "" lexbuf }

and comment depth pos = parse
  | open_comment    { cinc 2; comment (depth+1) ((!line_count,!char_count-2)::pos) lexbuf }
  | close_comment   { cinc 2; if depth = 1 then token lexbuf 
    else match pos with
	(x::xs) -> comment (depth-1) xs lexbuf
      | _ -> raise (Failure "should not occur") }
  | eof             { let ((l,c)::xs) = pos in raise (OpenComm{line_num = l; char_num = c}) }
  | _               { cinc 1; comment depth pos lexbuf }

and string str = parse
  | "\""            { cinc 1; STRING(str) }
  | "\\"            { cinc 1; escape str lexbuf }
  | _ as c          { cinc 1; string (str ^ (String.make 1 c)) lexbuf }

and escape str = parse
  | "\\"            { cinc 1; string (str ^ (String.make 1 '\\')) lexbuf }
  | "\""            { cinc 1; string (str ^ (String.make 1 '\"')) lexbuf }
  | "\\'"           { cinc 1; string (str ^ (String.make 1 '\'')) lexbuf }
  | "t"             { cinc 1; string (str ^ (String.make 1 '\t')) lexbuf }
  | "n"             { cinc 1; string (str ^ (String.make 1 '\n')) lexbuf }
  | "r"             { cinc 1; string (str ^ (String.make 1 '\r')) lexbuf }
  | (numeric)(numeric)(numeric) as num      { cinc 3; let n = int_of_string(num) in if (n > 255)
    then string (str ^ "\\" ^ num) lexbuf
    else string (str ^ (String.make 1 (char_of_int (int_of_string num)))) lexbuf }
  | _ as c          { print_int !char_count; cinc 2; string (str ^ (String.make 1 '\\') ^ (String.make 1  c)) lexbuf }

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

