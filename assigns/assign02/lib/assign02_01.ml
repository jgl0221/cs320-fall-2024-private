(*Problem 1: Tic-Tac-Toe*)
type piece = 
| X
| O

type pos = 
| Piece of piece
| Blank

type board = (pos * pos * pos) * (pos * pos * pos) * (pos * pos * pos)

type row_index = 
| Top
| Middle
| Bottom

type col_index = 
| Left
| Middle
| Right

type pos_index = row_index * col_index

let get_pos b (r_index, c_index)=
  (* finds row using matching with row index *)
  let findrow r_index (row1, row2, row3) =
    match r_index with
    | Top -> row1
    | Middle -> row2
    | Bottom -> row3
  in
  (* finds column using matching with column index within the row *)
  let findcol c_index (col1, col2, col3) =
    match c_index with
    | Left -> col1
    | Middle -> col2
    | Right -> col3
  in
  (* find row within board, tuple of b represents row 1 2 and 3, match row to Top, Middle or Bottom*)
  let row = findrow r_index b in
  (* find column/position within the row found previously, indexes into the correct position *)
  findcol c_index row

let threeinarow (p1, p2, p3) =
(* checks if the tuple has three in a row, wildcard takes care of blank leading to false *)
  match p1, p2, p3 with
  | Piece O, Piece O, Piece O -> true  
  | Piece X, Piece X, Piece X -> true
  | _ -> false

let winner b =
  let (r1, r2, r3) = b in
    (* rows *)
  threeinarow r1 || threeinarow r2 || threeinarow r3 ||
    (* columns *)
  threeinarow (get_pos b (Top, Left), get_pos b (Middle, Left), get_pos b (Bottom, Left)) ||
  threeinarow (get_pos b (Top, Middle), get_pos b (Middle, Middle), get_pos b (Bottom, Middle)) ||
  threeinarow (get_pos b (Top, Right), get_pos b (Middle, Right), get_pos b (Bottom, Right)) ||
    (* diagonals *)
  threeinarow (get_pos b (Top, Left), get_pos b (Middle, Middle), get_pos b (Bottom, Right)) ||
  threeinarow (get_pos b (Top, Right), get_pos b (Middle, Middle), get_pos b (Bottom, Left))
  