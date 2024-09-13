open Assign01_02

let nth s i =
  let prime = nth_prime i 
  in
    let rec exponent s prime = 
      if s mod prime = 0 then 1 + exponent (s/prime) prime 
      else 0
in exponent s prime