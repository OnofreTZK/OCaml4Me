(** My Solution - Solved *)

let rec gcd n1 n2 =
  if n2 = 0 then n1
  else gcd n2 (n1 mod n2);;

(** OCaml.or Solution *)

let rec gcd a b =
    if b = 0 then a else gcd b (a mod b);;
