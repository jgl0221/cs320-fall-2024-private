%{
open Utils

let rec mk_app e es =
  match es with
  | [] -> e
  | x :: es -> mk_app (App (e, x)) es
%}
%token LET
%token <string> VAR
%token <int> NUM
%token EQUALS
%token IN
%token EOF
%token ADD
%token SUB
%token MUL
%token DIV
%token LPAREN
%token RPAREN

%left ADD SUB
%left MUL DIV

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
  | LET; x = VAR; arg = VAR?; EQUALS; e1 = expr; IN; e2 = expr
    { match arg with
      | None -> Let (x, e1, e2)
      | Some arg -> LetFun (x, arg, e1, e2)
    }
  | e = expr1 { e }

%inline bop:
  | ADD { Add }
  | SUB { Sub }
  | MUL { Mul }
  | DIV { Div }

expr1:
  | e1 = expr1; op = bop; e2 = expr1 { Bop (op, e1, e2) }
  | e = expr2; es = expr2* { mk_app e es }

expr2:
  | n = NUM { Num n }
  | x = VAR { Var x }
  | LPAREN; e = expr; RPAREN { e } 

