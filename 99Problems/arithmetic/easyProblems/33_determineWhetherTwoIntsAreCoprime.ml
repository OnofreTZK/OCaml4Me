(** My Solution - Solved *)

let rec gcd n1 n2 =
  if n2 = 0 then n1
  else gcd n2 (n1 mod n2);;

let coprime a b =
  if gcd a b = 1 then true
  else false;;

(** OCaml.org Solution *)

let coprime a b = gcd a b = 1;;
