type ident = string

type ty = 
  | Unit
  | Arr of ty * ty  

type expr = 
  | Var of ident
  | Fun of ident * ty * expr
  | App of expr * expr

type ctxt = (ident * ty) list

let rec lookup (x : ident) (gamma : ctxt) : ty option =
  match gamma with
  | [] -> None
  | (y, t) :: rest -> if x = y then Some t else lookup x rest

let rec type_of (gamma : ctxt) (e : expr) : ty option =
  match e with
  | Var x -> lookup x gamma 

  | Fun (x, ty_x, body) ->
      (match type_of ((x, ty_x) :: gamma) body with
      | Some ty_body -> Some (Arr (ty_x, ty_body))
      | None -> None)

  | App (e1, e2) ->
      (match type_of gamma e1, type_of gamma e2 with
      | Some (Arr (ty_arg, ty_res)), Some ty_e2 when ty_arg = ty_e2 -> Some ty_res
      | _ -> None)