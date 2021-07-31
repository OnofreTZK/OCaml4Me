(** My Solution - Solved *)

let modulo x y =
  let result = x mod y in
  if result >= 0 then result
  else result + y;;

let is_prime num =
  let rec aux count div_count =
    if num = 1 then false
    else if div_count = 2 && count = 0 then true
    else if div_count > 2 then false
    else if (modulo num count) = 0 then aux (count-1) (div_count+1)
    else aux (count-1) div_count in
  aux num 0;;

(** OCaml.org *)

let is_prime n =
    let n = abs n in
    let rec is_not_divisor d =
      d * d > n || (n mod d <> 0 && is_not_divisor (d + 1)) in
    n <> 1 && is_not_divisor 2;;

(** Elegance *) 
