let lifespan f s p =
  let rec helper i current = 
    if p current then i - 1 (* return true, stop and return i - 1 because that's the last step that returned false *)
    else helper (i + 1) (f current)  (* false, continue running function *)
  in
  helper 1 (f s) (* i is set to 1, apply function f to s *)

let last_function_standing funcs start pred = (* inputs a bunch of functions funcs, starting value start, and predicate function to end the function*)
  let rec find_lifespans funcs =
    match funcs with 
    | [] -> []  
    | f :: fs -> (* take head function and leave rest of functions *)
        let lifespan_value = lifespan f start pred in (* calculate number of steps to determine lifespan value *)
        (f, lifespan_value) :: find_lifespans fs  (* append to list of tuples that contain functions f and lifespan values *)
  in
  let lifespans = find_lifespans funcs in  
  match lifespans with
  | [] -> None  (* if list is empty, retune None *)
  | [ (f, _) ] -> Some f  (* one function, return that Some of one function *)
  | _ -> 
      let max_lifespan = List.fold_left (fun acc (_, l) -> max acc l) min_int lifespans in (* List fold left compares all functions and finds maximum *)
      let candidates = List.filter (fun (_, l) -> l = max_lifespan) lifespans in (* initializes accumulator acc, checks any lifespan value is larger *)
      match candidates with
      | [ (f, _) ] -> Some f (* exactly one function with the maximum number of steps, return that function some of f *)
      | _ -> None  (* if not (tie) return None *)