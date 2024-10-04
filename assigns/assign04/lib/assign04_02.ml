type expr = 
| True
| False
| Num of int
| Or of expr * expr
| Add of expr * expr
| IfThenElse of expr * expr * expr

(* base types are all either Int or Bool *)
type ty = 
| Int
| Bool


(* checks expression if valid type according to our rules *)
let rec type_of expr = 
  match expr with
  | True -> Some Bool (* expr True is a Bool *)          
  | False -> Some Bool (* expr False is a Bool *)         
  | Num _ -> Some Int (* expr of Num with an int after is an Int *)         
  | Or(e1, e2) -> (* checks typing of expr 1 and expr 2, if both result in Some Bool return Some Bool because it is a valid expression *)              
    (match type_of e1, type_of e2 with
     | Some Bool, Some Bool -> Some Bool
     | _ -> None)
  | Add(e1, e2) -> (* checks typing of expr 1 and expr 2, to be valid Add must add two Ints to return Int. Checks if both e1 and e2 are Some Int*)            
    (match type_of e1, type_of e2 with
     | Some Int, Some Int -> Some Int
     | _ -> None)
  | IfThenElse(condition, e_then, e_else) ->  (* condition must be checked if Bool, then and else expressions checked for typing. 
  If both are type Int or both type Bool then return typing of then expression (could've also returned the else expression)*)
    (match type_of condition, type_of e_then, type_of e_else with
     | Some Bool, Some ty_then, Some ty_else when ty_then = ty_else -> Some ty_then
     | _ -> None)
  