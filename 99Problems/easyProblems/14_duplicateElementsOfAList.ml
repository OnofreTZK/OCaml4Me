(** My Solution with normal recursion - Solved *)

let rec duplicate = function
  | [] -> []
  | [element] -> [element] @ [element]
  | hd :: tl -> duplicate [hd] @ duplicate tl;;

(** My Solution with tail recursion - Solved *)

let tail_duplicate list =
  let rec aux acc current = 
    match current with
    | [] -> acc
    | [element] -> [element] @ acc
    | hd :: tl -> [hd] @ [hd] @ aux acc tl in
  aux [] list;;

(** OCaml.org Solution *)

let rec duplicate = function
    | [] -> []
    | h :: t -> h :: h :: duplicate t;;

(** Not tail recursive *)


