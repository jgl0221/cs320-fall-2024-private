open Utils

let rec eval e = 
  match e with
  | Num n -> VNum n  (* Directly evaluate numbers to their corresponding value *)
  | Add (e1, e2) ->
      (match eval e1, eval e2 with
       | VNum n1, VNum n2 -> VNum (n1 + n2)  (* Sum two evaluated numbers *)
       | _ -> failwith "Type error in addition")  (* Shouldn't happen if well-typed *)
  | Lt (e1, e2) ->
      (match eval e1, eval e2 with
       | VNum n1, VNum n2 -> VBool (n1 < n2)  (* Compare two evaluated numbers *)
       | _ -> failwith "Type error in comparison")  (* Shouldn't happen if well-typed *)
  | Ite (cond, then_branch, else_branch) ->
      (match eval cond with
       | VBool true -> eval then_branch  (* Evaluate the then branch if condition is true *)
       | VBool false -> eval else_branch  (* Evaluate the else branch if condition is false *)
       | _ -> failwith "Condition must be a boolean")  (* Shouldn't happen if well-typed *)

