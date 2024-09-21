type matrix = {
entries : float list list;
rows : int;
cols : int;
}

(* recursive function goes through entries, returns empty list if no entries*)
let rec sublists list n = 
  match list with
  | [] -> []
  | _ ->
(* else create rows with the first n entries, and make rest list with n entries. concatenate to row. List.take creates
a new list, so every n entries taken is a new row *)
    let row = List.take n list in
    let rest = List.drop n list in
    row :: sublists rest n

let mk_matrix entries (r, c) =
(* set n to be c which represents number of columns. number of columns represents number of values within each tuple/row *)
  {
    entries = sublists entries c;
    rows = r;
    cols = c;
  }