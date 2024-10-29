let rec incr_list (xs: int list): int list = 
    match xs with
    | [] -> []
    | x :: xs -> x + 1 :: incr_list xs

let rec map (f: 'a -> 'b) (xs: 'a list) : 'b list =
    match xs with 
    | [] -> []
    | x :: xs -> f x :: map f xs

let incr_list (xs: int list)
    map (fun x -> x + 1) xs

(* [f(x) for x in xs]*)

let string_of_intlist (xs: int list) : string list = 
    map string_of_int xs

(* fold_left *)

let sum_list (xs: int list) : int = 
    let rec loop (acc: int) (xs: int list) : int =
        match xs with
        | [] -> acc
        | x :: xs -> loop (acc + x) xs
    in 
    loop 0 xs

let rec fold left (f: 'a -> 'b -> 'a) (acc: 'a) (xs : 'b list) : 'a =
    match xs with
    | [] -> acc
    | x :: xs -> fold_left f (f acc x) xs

let sum_list (xs: int list) =
    fold_left (fun acc x -> acc + x) 0 xs

(*

acc = 0
for x in xs:
  acc = acc + x
return acc 

*)

(* dot product *)

let rec zip (xs: 'a list) (ys: 'b list) : (a' * b') list =
  match xs, ys with
  | x :: xs, y :: ys -> (x, y) :: zip xs ys
  | _ -> []

let dot_product (xs: int list) (ys: int list) : int =
  sum list(map (fun (x, y) -> x * y) zip xs ys)

let dot_product (xs: int list) (ys: int list) : int =
  zip xs ys |> 
    map (fun (x, y) -> x * y) |>
      sum _list 

let dot produce (xs: int list) (ys: int list) : int =
  sum_list @@ (*@@ basically surrounds the items to the right/below in parentheses *)
    List.map2 (fun x y -> x * y) xs ys (* List.map2 does the zipping and mapping above *)

let max_list (xs: int list): int =
  fold_left (fun opt x -> 
    match opt with
    | None -> Some x
    | Some y -> Some (max x y))
  None xs
(* none means failure, some means success. the types for option types *)

let avg list (xs : int list) : float =
  let sum = sum_list xs in 
  let count = fold_left (fun acc _ -> acc + 1) 0 xs in 
  float_of_int sum /. float_of_int count

let avg list (xs : int list) : float =
  let sum, count =
    fold_left (fun (sum, count) x ->
        (sum + x, count + 1))
      (0, 0) xs
    in 
    float_of_int sum /. float_of_int count

let rec filter (f: a' -> bool) (xs: 'a list) : a' list = 
  match xs with
  | x :: xs ->
    if f x then x :: filter f xs
    else filter f xs
  | [] -> []

let upper half (xs: int list) : int list =
  let avg = avg list xs in 
  filter (fun x -> avg <= float_of_int x) xs

type ('k, 'v) dict = ('k * 'v) list

let find (k: 'k) (d : ('k, 'v) dict) : 'v =
  fold_left (fun opt (k', v') ->
    match opt with 
    | None -> 
        if k = k' then Some v
        else None
    | Some _ -> opt)
  None d

let test = [("a", 1); ("b", 2); ("c", 3)]

let rec string_of_list (f: 'a -> string) (xs: 'a list) : string =
  match xs with
  | [] -> "[]"
  | x :: xs -> f x ^ " :: " ^ string_of_list f xs

let rec fold_right (f : 'a -> 'b -> 'b) (xs :'a list) (acc : 'b) : 'b =
  match xs with 
  | [] -> acc
  | x :: xs ->
      f x (fold_right f xs acc)

let string_of_list (f: 'a -> string) (xs: 'a list) : string =
  fold right (fun x acc -> f x ^ " :: " ^ acc) xs "[]"

  
