let rec addlist key value list =
  match list with
  | [] -> [(key, value)] (* add to new list *)
  | (k, v) :: rest -> (* check if key value matches one of pairs in new list*)
      if k = key then (k, v + value) :: rest (* if key exists, add current key's value *)
      else (k, v) :: (addlist key value rest) (* if not, recurse the rest of the list *)

let rec mk_unique_keys alst =
  match alst with
  | [] -> [] (* return empty list if alst is empty *)
  | (key, value) :: rest -> (* take first key value pair in the list *)
      let updated_rest = mk_unique_keys rest in (* recurse through rest of list *)
      addlist key value updated_rest (* add key value to updated list *)