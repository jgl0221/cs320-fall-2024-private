let rec sum_last_n lst n =
  match lst with
  | [] -> 0  (* if the list is empty return 0 *)
  | _ when n = 0 -> 0  (* no more elements return 0 *)
  | x :: xs -> x + sum_last_n xs (n - 1)  (* recurse and sum elements *)

let gen_fib l k =
  let len_l = List.length l in  (* length of the input list *)
  let rec recurse current_list n =
    if n = k then
      match current_list with
      | [] -> 0  (* if empty return 0 *)
      | x :: _ -> x  (* return the first element of the current_list *)
    else if n < len_l then
      List.nth l n  (* return element at index n if less than length l *)
    else
      let new_val = sum_last_n current_list len_l in  (* compute the next value by summing the last len_l elements *)
      recurse (current_list @ [new_val]) (n + 1)  (* append the new value and continue *)
  in
  recurse l 0  (* start recursion with the initial list and index 0 *)
