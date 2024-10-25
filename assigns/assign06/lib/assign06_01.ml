open Utils

let lex s =
  let words = split s in
  let rec translate words acc = 
    match words with
    | [] -> Some (List.rev acc)
    | tk :: tks ->
      match tok_of_string_opt tk with 
      | Some token -> translate tks (token :: acc)
      | None -> None
    in translate words [] 