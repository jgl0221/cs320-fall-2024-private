let is_prime n =
  let rec divisor d =
    if d * d > n then true
    else if n mod d = 0 then false
    else divisor (d + 1)
  in if n < 2 then false else divisor 2

let nth_prime n =
  let rec find_prime number value =
    if is_prime value then
      if number = n then value
      else find_prime (number + 1) (value + 1)
    else find_prime number (value + 1)
  in
  find_prime 0 2