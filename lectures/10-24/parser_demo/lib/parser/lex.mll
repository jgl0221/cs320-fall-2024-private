{
open Par
}

let whitespace = [' ' '\n' '\t' '\r']+
<<<<<<< HEAD
(* decides what to do with regular expressions: numbers, variables *)
=======
>>>>>>> upstream/main
let num = '-'? ['0'-'9']+
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*


rule read = (* tells us which token each possible string associates to *)
  parse
  | "let" { LET }
  | "=" { EQUALS }
  | "in" { IN }
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "/" { DIV }
  | "(" { LPAREN }
  | ")" { RPAREN }
<<<<<<< HEAD
  | num { NUM (int_of_string(Lexing.lexeme lexbuf))} (* lexbuf is the string we're reading *)
  | var { VAR (Lexing.lexeme lexbuf)}
=======
  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | var { VAR (Lexing.lexeme lexbuf) }
>>>>>>> upstream/main
  | whitespace { read lexbuf }
  | eof { EOF }
