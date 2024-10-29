%{
open Utils
%}
%token LET
%token <string> VAR
%token <int> NUM
%token EQUALS
%token IN
%token EOF (*end of file token *)
%token ADD
%token SUB
%token MUL
%token DIV
%token LPAREN
%token RPAREN
%left ADD SUB
%left MUL DIV
(* %left or right determines precendence/associativity *)

%start <Utils.prog> prog

%%

prog:
  | e = expr; EOF { e }

expr:
  | LET; x = VAR; EQUALS; e1 = expr; IN; e2 = expr { Let (x, e1, e2) } (* can add matching within the curly braces *)
  | e = expr1 {e}
(* put production rule in curly braces. Lowercase Let is for the constructor of the ADT*)
(* go through all the rules, create the new tokens we need *)

%inline bop: 
  | ADD { Add }
  | SUB { Sub }
  | MUL { Mul }
  | DIV { Div }


expr1: 
  | e1 = expr1; op = bop; e2 = expr1 { Bop (op, e1, e2) }
  | n = NUM { Num n }
  | x = VAR { Var x }
  | LPAREN; e = expr; RPAREN { e }