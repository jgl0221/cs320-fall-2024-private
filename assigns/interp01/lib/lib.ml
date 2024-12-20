open Utils

(*Parse*)
let parse s = My_parser.parse s

(*Subst*)
let expr_of_value v =
  match v with
  | VFun (x, e) -> Fun (x, e)
  | VNum n -> Num n
  | VBool true -> True
  | VBool false -> False
  | VUnit -> Unit

let rec var_replace y x e = 
  match e with 
  | Num n -> Num n
  | True -> True
  | False -> False
  | Unit -> Unit
  | Var z -> if z = x then Var y else Var z
  | Bop (op, e1, e2) -> Bop (op, var_replace y x e1, var_replace y x e2)
  | If (e1, e2, e3) -> If (var_replace y x e1, var_replace y x e2, var_replace y x e3)
  | Let (z, e1, e2) -> if z = x then Let (z, var_replace y x e1, e2) else Let (z, var_replace y x e1, var_replace y x e2)
  | App (e1, e2) -> App (var_replace y x e1, var_replace y x e2)
  | Fun (z, e) -> if z = x then Fun (z, e) else Fun (z, var_replace y x e)

let rec subst v x e =
  match e with
  | Num n -> Num n
  | True -> True
  | False -> False
  | Unit -> Unit
  | Var y -> if x = y then (expr_of_value v) else Var y
  | Bop (op, e1, e2) -> Bop (op, subst v x e1, subst v x e2)
  | If (e1, e2, e3) -> If (subst v x e1, subst v x e2, subst v x e3)
  | Let (y, e1, e2) -> if x = y then Let (y, subst v x e1, e2) else Let (y, subst v x e1, subst v x e2)
  | App (e1, e2) -> App (subst v x e1, subst v x e2)
  | Fun (y, e) ->
    if x = y
    then Fun (y, e)
    else 
      let y' = gensym () in
      Fun (y', subst v x (var_replace y' y e))

(*Eval*)
let rec eval e =
  match e with
  | Num n -> Ok (VNum n)
  | True -> Ok (VBool true)
  | False -> Ok (VBool false)
  | Unit -> Ok VUnit
  | Var x -> Error (UnknownVar x)
  | Bop (op, e1, e2) -> (
    match op with
    | And -> (
          match eval e1 with
          | Ok (VBool false) -> Ok (VBool false) 
          | Ok (VBool true) -> eval e2           
          | Ok _ -> Error (InvalidArgs op)       
          | Error e -> Error e)
    | Or -> (
          match eval e1 with
          | Ok (VBool true) -> Ok (VBool true)
          | Ok (VBool false) -> eval e2
          | Ok _ -> Error (InvalidArgs op)
          | Error e -> Error e)
    | _ -> (
      match (eval e1, eval e2) with
      | Ok (VNum n1), Ok (VNum n2) -> (
        match op with
        | Add -> Ok (VNum (n1 + n2))
        | Sub -> Ok (VNum (n1 - n2))
        | Mul -> Ok (VNum (n1 * n2))
        | Div -> if n2 = 0 then Error DivByZero else Ok (VNum (n1 / n2))
        | Mod -> if n2 = 0 then Error DivByZero else Ok (VNum (n1 mod n2))
        | Lt -> Ok (VBool (n1 < n2))
        | Lte -> Ok (VBool (n1 <= n2))
        | Gt -> Ok (VBool (n1 > n2))
        | Gte -> Ok (VBool (n1 >= n2))
        | Eq -> Ok (VBool (n1 = n2))
        | Neq -> Ok (VBool (n1 <> n2))
        | _ -> Error (InvalidArgs op))
    | Ok _, Ok _ -> Error (InvalidArgs op)    
    | Error e1, _ -> Error e1
    | _, Error e2 -> Error e2))
  | If (e1, e2, e3) -> (
    match eval e1 with
    | Ok (VBool true) -> eval e2
    | Ok (VBool false) -> eval e3
    | Ok _ -> Error InvalidIfCond
    | Error e -> Error e)
  | Let (x, e1, e2) -> (
    match eval e1 with
    | Ok v -> eval (subst v x e2)
    | Error e -> Error e)
  | Fun (x, e) -> Ok (VFun (x, e))  
  | App (e1, e2) -> (
    match eval e1 with
    | Ok (VFun (x, e)) -> (
      match eval e2 with
      | Ok v2 -> eval (subst v2 x e)
      | Error e -> Error e)
    | Ok _ -> Error InvalidApp  
    | Error e -> Error e)

    
(*Interp*)
let interp s =
  match parse s with
    | Some expr -> eval expr
    | None -> Error ParseFail
        