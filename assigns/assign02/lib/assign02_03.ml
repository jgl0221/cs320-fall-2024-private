type dir = 
  | North
  | South
  | East
  | West

type path = dir list

let dist directions =
  (* Helper function to update coordinates based on direction *)
  let rec walk directions (x, y) =
    match directions with
    | [] -> (x, y)
    | North :: rest -> walk rest (x, y + 1)
    | South :: rest -> walk rest (x, y - 1)
    | East  :: rest -> walk rest (x + 1, y)
    | West  :: rest -> walk rest (x - 1, y)
  in
  let (x, y) = walk directions (0, 0) in
  sqrt (float_of_int (x * x + y * y))