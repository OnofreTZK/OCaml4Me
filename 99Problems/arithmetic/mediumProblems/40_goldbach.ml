(* My Solution - Solved*) 

let is_prime n =
    let n = abs n in
    let rec is_not_divisor d =
      d * d > n || (n mod d <> 0 && is_not_divisor (d + 1)) in
    n <> 1 && is_not_divisor 2;;

let tuple_update (fst, snd) value ctrl =
  if ctrl then (value, snd) else (fst, value);;

let goldbach value =
  let rec aux fst snd tupl =
    let () = Printf.printf "%d + %d = %d\n" fst snd (fst+snd) in
    if value mod 2 <> 0 then tupl
    else if fst = 0 || snd = value then tupl
    else if fst + snd = value && is_prime fst && is_prime snd then (fst, snd) (*hit*)
    else if is_prime fst && is_prime snd then aux (fst-1) (snd+1) (fst, snd)(*both prime*)
    else if is_prime fst && not (is_prime snd) then aux (fst-1) (snd+1) (tuple_update tupl fst true)(*fst prime*)
    else if not (is_prime fst) && is_prime snd then aux (fst-1) (snd+1) (tuple_update tupl snd false)(*snd prime*)
    else aux (fst-1) (snd+1) tupl in
  aux value 0 (0, 0);;

(* OCaml.org Solution *)

let goldbach n =
    let rec aux d =
      if is_prime d && is_prime (n - d) then (d, n - d) (*So obviousssss*)
      else aux (d + 1)
    in
      aux 2;;

(* Wow, just this kkkkkkk *)
(* feeling dumb *)

    
