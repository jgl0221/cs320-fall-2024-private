%{
open Utils

let rec mk_app e es =
  match es with
  | [] -> e
  | x :: es -> mk_app (App (e, x)) es

%}

%token <int> NUM
%token <string> VAR
%token EOF
%token LET "let"
%token COLON ":"
%token EQUALS "="
%token REC "rec"
%token LPAREN "("
%token RPAREN ")"
%token INTTY "int"
%token BOOLTY "bool"
%token UNITTY "unit"
%token ARROW "->"
%token IN "in"
%token IF "if"
%token THEN "then"
%token ELSE "else"
%token FUN "fun"
%token ASSERT "assert"
%token UNIT "()"
%token TRUE "true"
%token FALSE "false"
%token PLUS "+"
%token MINUS "-"
%token TIMES "*"
%token DIVIDE "/"
%token MOD "%"
%token LT "<"
%token LTE "<="
%token GT ">"
%token GTE ">="
%token NOTEQUALS "<>"
%token AND "&&"
%token OR "||"

%right OR
%right AND
%left LT LTE GT GTE EQUALS NOTEQUALS
%left PLUS MINUS 
%left TIMES DIVIDE MOD

%start <Utils.prog> prog

%%

prog:
  | ls = toplet* EOF { ls }

toplet:
  | LET; x = VAR; arg = arg; COLON; ty = ty; EQUALS; e = expr { TopLet(x, arg, ty, expr) }
  | LET; REC; x = VAR; arg1 = arg; arg2 = arg; COLON; ty = ty; EQUALS; e = expr { TopLetRec(x, arg1, arg2, ty, expr) }
  | LET; x = VAR; arg = arg; COLON; ty = ty; EQUALS; e = expr { List.fold_right (fun (v, t) acc -> Fun((v, t), acc)) arg (TopLet(x, [], ty, e)) }
  | LET; REC; x = VAR; arg = arg; COLON; ty = ty; EQUALS; e = expr { List.fold_right (fun (v, t) acc -> Fun((v, t), acc)) arg (TopLetRec(x, [], ty, e)) }

arg:
  | LPAREN; v = VAR; COLON; ty = ty; RPAREN { (v, ty) }

ty:
  | INTTY { IntTy }
  | BOOLTY { BoolTy }
  | UNITTY { UnitTy }
  | ty1 = ty; ARROW; ty2 = ty { FunTy (ty1, ty2) }
  | LPAREN; ty = ty; RPAREN { ty }

expr:
  | LET; x = VAR; arg = arg; COLON; ty = ty; EQUALS; e1 = expr; IN; e2 = expr { List.fold_right (fun (v, t) acc -> Fun((v, t), acc)) arg (Let(x, [], ty, e1, e2)) }
  | LET; REC; x = VAR; arg = arg; COLON; ty = ty; EQUALS; e1 = expr; IN; e2 = expr { List.fold_right (fun (v, t) acc -> Fun((v, t), acc)) arg (LetRec(x, [], ty, e1, e2)) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If(e1, e2, e3) }
  | FUN; arg = arg; ARROW; e = expr { List.fold_right (fun (v, ty) acc -> Fun((v, ty), acc)) arg e }
  | e = expr2 { e }

expr2:
  | e1 = expr2; op = bop; e2 = expr2 { Bop(op, e1, e2) }
  | ASSERT; e3 = expr3 { Assert(e3) }
  | e1 = expr3; e2 = expr3 { mk_app e1 e2 }

expr3:
  | x = VAR { Var x }
  | n = NUM { Num n }
  | TRUE { True }
  | FALSE { False }
  | LPAREN; RPAREN { Unit }
  | LPAREN; e = expr; RPAREN { e }

%inline bop:
  | PLUS{ Add }
  | MINUS { Sub }
  | TIMES { Mul }
  | DIVIDE { Div }
  | MOD { Mod }
  | LT { Lt }
  | LTE { Lte }
  | GT { Gt }
  | GTE { Gte }
  | EQUALS { Eq }
  | NOTEQUALS { Neq }
  | AND { And }
  | OR { Or }




