type tree = 
  | Leaf of int
  | Node of tree list

let rec height t =
  match t with
  | Leaf _ -> 0
  | Node cs ->
     let rec max_depth cs =
       match cs with
       | [] -> -1
       | c :: cs -> max (height c) (max_depth cs)
     in 1 + max_depth cs

let rec collect_terminals t =
  match t with
  | Leaf _ as leaf -> [leaf]  (* leaf is a terminal element *)
  | Node [] as empty_node -> [empty_node]  (* nodes are terminal elements *)
  | Node children -> List.concat (List.map collect_terminals children)
  (* traverse through all children, get all their terminals *)

let rec collapse h t =
  if h <= 0 then t  (* height is less than or equal to 0, no tree *)
  else
    match t with
    | Leaf _ -> t  (* leaf doesn't need to be collapsed *)
    | Node [] -> t  (* empty node doesn't need to be collapsed *)
    | Node children ->
        if h = 1 then Node (collect_terminals t)  (* collapsing to height of 1 means replace all children with terminals *)
        else Node (List.map (collapse (h-1)) children)  (* recursively collapse the children *)
