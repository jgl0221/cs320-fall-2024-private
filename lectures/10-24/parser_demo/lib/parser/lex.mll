{
open Par
}

let whitespace = [' ' '\n' '\t' '\r']+
(* decides what to do with regular expressions: numbers, variables *)
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
  | num { NUM (int_of_string(Lexing.lexeme lexbuf))} (* lexbuf is the string we're reading *)
  | var { VAR (Lexing.lexeme lexbuf)}
  | whitespace { read lexbuf }
  | eof { EOF }
