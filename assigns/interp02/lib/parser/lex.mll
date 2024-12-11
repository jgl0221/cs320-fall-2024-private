{
open Par
}

let whitespace = [' ' '\t' '\n' '\r']+
let num = '-'? ['0'-'9']+
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read = (* tells us which token each possible string associates to *)
  parse
  | "let" { LET }
  | ":" { COLON }
  | "=" { EQUALS }
  | "rec" { REC }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "int" { INTTY }
  | "bool" { BOOLTY }
  | "unit" { UNITTY }
  | "->" { ARROW }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "fun" { FUN }
  | "assert" { ASSERT }
  | "()" { UNIT }
  | "true" { TRUE }
  | "false" { FALSE }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIVIDE }
  | "mod" { MOD }
  | "<" { LT }
  | "<=" { LTE }
  | ">" { GT }
  | ">=" { GTE }
  | "<>" { NOTEQUALS }
  | "&&" { AND }
  | "||" { OR }

  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | var { VAR (Lexing.lexeme lexbuf) }
  | whitespace { read lexbuf }
  | eof { EOF }

