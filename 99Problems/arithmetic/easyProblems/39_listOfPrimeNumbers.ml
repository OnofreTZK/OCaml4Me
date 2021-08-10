(* My Solution - Solved *)

let is_prime n =
    let n = abs n in
    let rec is_not_divisor d =
      d * d > n || (n mod d <> 0 && is_not_divisor (d + 1)) in
    n <> 1 && is_not_divisor 2;;

let all_primes first last =
  let rec aux acc count =
    if count = last then acc
    else if is_prime count then aux (count :: acc) (count+1)
    else aux acc (count+1) in
  aux [] first;;

(* OCaml.org Solution *)
let is_prime n =
    let n = max n (-n) in
    let rec is_not_divisor d =
      d * d > n || (n mod d <> 0 && is_not_divisor (d + 1))
    in
      is_not_divisor 2
  
let rec all_primes a b =
  if a > b then [] else
    let rest = all_primes (a + 1) b in
    if is_prime a then a :: rest else rest;;

