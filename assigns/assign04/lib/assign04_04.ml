type ident = string

type expr' = 
| True
| False
| Num of int
| Var of ident
| Let of ident * expr' * expr'
| Add of expr' * expr'
| Or of expr' * expr'
| IfThenElse of expr' * expr' * expr'

type ty' = (* two types Int and Bool *)
| Int
| Bool

type context = (ident * ty') list (* pairs variables with their type *)


let rec lookup (x: ident) (ctx: context) : ty' option =
  match ctx with (* checks context, if there's a variable of that type return Some that type t. If not, return None *)
  | [] -> None
  | (y, t) :: rest -> if x = y then Some t else lookup x rest

let rec type_of' (gamma: context) (e: expr') : ty' option = (* gamma is our context, e is our expression *)
  match e with
  | True -> Some Bool (* True and False are always Bool *)
  | False -> Some Bool
  | Num _ -> Some Int (* Num is always an Int *)
  | Var x -> lookup x gamma (* if x is in context gamma, returns type *)
  | Let (x, e1, e2) -> 
      (match type_of' gamma e1 with (* check e1 is well-typed. if it is, returns Some type and then with new context check body expression 2 *)
      | Some t1 ->
          let new_gamma = (x, t1) :: gamma in
          type_of' new_gamma e2
      | None -> None) (* else return None *)
  | Add (e1, e2) ->
      (match type_of' gamma e1, type_of' gamma e2 with (* check e1 and e2 typing given context gamma, if both Int return Some Int *)
      | Some Int, Some Int -> Some Int
      | _ -> None)
  | Or (e1, e2) -> (* same as Add, check typings and if both are Bool return Some Bool *)
      (match type_of' gamma e1, type_of' gamma e2 with
      | Some Bool, Some Bool -> Some Bool
      | _ -> None)
  | IfThenElse (cond, then_branch, else_branch) -> (* check typing of condition cond, if Bool check typing of then and else branches. If typings are same, return typing of then branch *)
      match type_of' gamma cond with
      | Some Bool ->
          (match type_of' gamma then_branch, type_of' gamma else_branch with
          | Some t1, Some t2 when t1 = t2 -> Some t1
          | _ -> None)
      | _ -> None