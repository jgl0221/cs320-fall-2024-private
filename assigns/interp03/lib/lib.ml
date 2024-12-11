open Utils
include My_parser

(* Custom implementation of unify and type_of functions *)

let rec apply_subst subst t =
  match t with
  | TVar x -> (try List.assoc x subst with Not_found -> t)
  | TFun (t1, t2) -> TFun (apply_subst subst t1, apply_subst subst t2)
  | TPair (t1, t2) -> TPair (apply_subst subst t1, apply_subst subst t2)
  | TList t -> TList (apply_subst subst t)
  | _ -> t

let compose_subst subst1 subst2 =
  List.map (fun (x, t) -> (x, apply_subst subst1 t)) subst2 @ subst1

let rec mem_assoc key lst =
  match lst with
  | [] -> false
  | (k, _) :: _ when k = key -> true
  | _ :: rest -> mem_assoc key rest

let rec unify_types subst t1 t2 =
  let t1 = apply_subst subst t1 in
  let t2 = apply_subst subst t2 in
  match t1, t2 with
  | TVar x, TVar y when x = y -> subst
  | TVar x, _ -> if mem_assoc x subst then unify_types subst (List.assoc x subst) t2 else (x, t2) :: subst
  | _, TVar y -> unify_types subst t2 t1
  | TFun (t1a, t1b), TFun (t2a, t2b)
  | TPair (t1a, t1b), TPair (t2a, t2b) -> unify_types (unify_types subst t1a t2a) t1b t2b
  | TList t1, TList t2 -> unify_types subst t1 t2
  | TInt, TInt | TBool, TBool | TFloat, TFloat -> subst
  | _ -> failwith "Unification failed"

let unify t constrs =
  try
    let subst = List.fold_left (fun acc (t1, t2) -> unify_types acc t1 t2) [] constrs in
    Some (apply_subst subst t)
  with Failure _ -> None

type 'a ref = { mutable contents : 'a }
let ref x = { contents = x }
let (!) r = r.contents
let (:=) r x = r.contents <- x

let gensym =
  let counter = ref 0 in
  fun () ->
    let name = !counter in
    counter := !counter + 1;
    TVar ("a" ^ string_of_int name)

let rec infer_type env expr =
  match expr with
  | Int _ -> (TInt, [])
  | True | False -> (TBool, [])
  | Var x ->
    (try
       match List.assoc x env with
       | Forall (_, ty) -> ty, []
     with Not_found -> failwith ("Unbound variable: " ^ x))
  | Fun (x, ty_opt, e) ->
    let arg_type = gensym () in
    let env' = (x, Forall ([], arg_type)) :: env in
      let body_type, constrs = infer_type env' e in
      (TFun (arg_type, body_type), constrs)
  | App (value, body) ->
    let t1, c1 = infer_type env value in
    let t2, c2 = infer_type env body in
      let result_type = gensym () in
      (result_type, (t1, TFun (t2, result_type)) :: (c1 @ c2))
  | Let { is_rec; name; value; body } ->
    let t1, c1 = infer_type env value in
    let env' = (name, Forall ([], t1)) :: env in
    let t2, c2 = infer_type env' body in
    (t2, c1 @ c2)
  | If (e1, e2, e3) ->
      let t1, c1 = infer_type env e1 in
      let t2, c2 = infer_type env e2 in
      let t3, c3 = infer_type env e3 in
      (t2, (t1, TBool) :: (t2, t3) :: (c1 @ c2 @ c3))
  | _ -> failwith "Unsupported expression"

let type_of env expr =
  try
    let t, constrs = infer_type env expr in
    match unify t constrs with
    | Some unified -> Some (Forall ([], unified))
    | None -> None
  with Failure _ -> None

exception AssertFail
exception DivByZero
exception RecWithoutArg
exception CompareFunVals

let eval_expr _ _ = assert false

let type_check =
  let rec go ctxt = function
  | [] -> Some (Forall ([], TUnit))
  | {is_rec;name;value} :: ls ->
    match type_of ctxt (Let {is_rec;name;value; body = Var name}) with
    | Some ty -> (
      match ls with
      | [] -> Some ty
      | _ ->
        let ctxt = Env.add name ty ctxt in
        go ctxt ls
    )
    | None -> None
  in go Env.empty

let eval p =
  let rec nest = function
    | [] -> Unit
    | [{is_rec;name;value}] -> Let {is_rec;name;value;body = Var name}
    | {is_rec;name;value} :: ls -> Let {is_rec;name;value;body = nest ls}
  in eval_expr Env.empty (nest p)

let interp input =
  match parse input with
  | Some prog -> (
    match type_check prog with
    | Some ty -> Ok (eval prog, ty)
    | None -> Error TypeError
  )
  | None -> Error ParseError
