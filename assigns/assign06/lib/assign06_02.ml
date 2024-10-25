open Utils

let parse toks =
  let rec process toks stack =
    match toks, stack with
    | [], [e] -> Some e  
    | TNum n :: tokens, _ -> process tokens (Num n :: stack)
    | TAdd :: tokens, e2 :: e1 :: rest -> process tokens (Add (e1, e2) :: rest)
    | TLt :: tokens, e2 :: e1 :: rest -> process tokens (Lt (e1, e2) :: rest)
    | TIte :: tokens, e3 :: e2 :: e1 :: rest -> process tokens (Ite (e1, e2, e3) :: rest)
    | _ -> None  
    in process toks []
