(* My Solution - Solved *)
(**************************************************************************************************)
let is_prime n =
    let n = abs n in
    let rec is_not_divisor d =
      d * d > n || (n mod d <> 0 && is_not_divisor (d + 1)) in
    n <> 1 && is_not_divisor 2;;

let goldbach n =
    let rec aux d =
      if is_prime d && is_prime (n - d) then (d, n - d)
      else aux (d + 1)
    in
      aux 2;;
(**************************************************************************************************)

let goldbach_list fst snd =
  let rec aux acc runner =
    if runner > snd then acc
    else if runner mod 2 = 0 then aux ((runner ,goldbach runner) :: acc) (runner+1)
    else aux acc (runner+1) in
  List.rev (aux [] fst);;

(* OCaml.org Solution *)

let rec goldbach_list a b =
    if a > b then [] else
      if a mod 2 = 1 then goldbach_list (a + 1) b
      else (a, goldbach a) :: goldbach_list (a + 2) b;;

let goldbach_limit a b lim =
    List.filter (fun (_, (a, b)) -> a > lim && b > lim) (goldbach_list a b);;
