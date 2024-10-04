open Assign04_02

type value = 
| VNum of int
| VBool of bool

exception InvalidExpression

let rec eval expr =
  match expr with
  | True -> VBool true (* True evaluates to true *)
  | False -> VBool false (* False evaluates to false *)
  | Num num -> VNum num (* Num evaluates to its value num *)
  | Or (e1, e2) ->
    (match eval e1, eval e2 with (* match e1 and e2 after evaluating them, do the OR operator on the two expressions *)
    | VBool b1, VBool b2 -> VBool (b1 || b2)
    | _ -> raise InvalidExpression)
  | Add (e1, e2) -> (* evaluate the two to see if both are num, then add them else raise exception *)
    (match eval e1, eval e2 with
    | VNum n1, VNum n2 -> VNum (n1 + n2)
    | _ -> raise InvalidExpression)
  | IfThenElse (condition, e_then, e_else) -> (* evaluate condition, depending on condition evaluate the then or else *)
    (match eval condition with
    | VBool true -> eval e_then
    | VBool false -> eval e_else
    | _ -> raise InvalidExpression) 