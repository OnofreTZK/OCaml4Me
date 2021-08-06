(* My Solution  - Partial Solved (Didn't add the last prime)*)

let factor n =
    let rec aux acc prime n_value =
        if n_value < (prime*prime) then acc
        else if n_value mod prime = 0 then aux (prime :: acc) prime (n_value/prime)
        else aux acc (prime+1) n_value in
    List.rev (aux [] 2 n);;

(* Second Try - Solved *)

let factor n =
    let rec aux acc prime n_value =
        if n_value = 1 then acc
        else if n_value mod prime = 0 then aux (prime :: acc) prime (n_value/prime)
        else aux acc (prime+1) n_value in
    List.rev (aux [] 2 n);;

(* OCaml.org Solution *)
 let factors n =
    let rec aux d n =
      if n = 1 then [] else
        if n mod d = 0 then d :: aux d (n / d) else aux (d + 1) n
    in
      aux 2 n;;
