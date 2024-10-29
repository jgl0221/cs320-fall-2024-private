type dir = 
  | North
  | South
  | East
  | West

type path = dir list

let dist directions =
(* recursively walk through the path and update coordinates using matching *)
  let rec walk directions (x, y) =
    match directions with
    | [] -> (x, y)
    | North :: rest -> walk rest (x, y + 1)
    | South :: rest -> walk rest (x, y - 1)
    | East  :: rest -> walk rest (x + 1, y)
    | West  :: rest -> walk rest (x - 1, y)
  in
(* initialize to the origin to make calculations easier *)
  let (x, y) = walk directions (0, 0) in 
(* finds the distance *)
  sqrt (float_of_int (x * x + y * y))