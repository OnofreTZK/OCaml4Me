(** My Solution - Solved *)

let insert_at value n list = 
  let size = List.length list in
  let rec aux count = function
    | [] -> [value]
    | [element] -> if count = n then value :: [element] else if n >= size then [element] @ [value] else [element]
    | hd :: tl -> if count = n then value :: [hd] @ aux (count+1) tl
                  else [hd] @ aux (count+1) tl in
  aux 0 list;;

(** OCaml.org Solution *)

let rec insert_at x n = function
    | [] -> [x]
    | h :: t as l -> if n = 0 then x :: l else h :: insert_at x (n - 1) t;;

(** So fancy *)
