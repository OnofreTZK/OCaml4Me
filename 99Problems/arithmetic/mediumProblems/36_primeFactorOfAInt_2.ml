(* My Solution - Solved *)

let encode list =
  let rec aux count = function
    | [] -> []
    | [element] -> [(element, count)]
    | hd :: tl ->
      if hd = List.hd tl then aux (count + 1) tl
      else [(hd, count)] @ aux 1 tl in
  aux 1 list;;

let factors n =
    let rec aux acc prime n_value =
        if n_value = 1 then acc
        else if n_value mod prime = 0 then aux (prime :: acc) prime (n_value/prime)
        else aux acc (prime+1) n_value in
    encode (List.rev (aux [] 2 n));;

(* OCaml.org Solution *)

let factors n =
    let rec aux d n =
      if n = 1 then [] else
        if n mod d = 0 then
          match aux d (n / d) with
          | (h, n) :: t when h = d -> (h, n + 1) :: t
          | l -> (d, 1) :: l
        else aux (d + 1) n
    in
      aux 2 n;;


