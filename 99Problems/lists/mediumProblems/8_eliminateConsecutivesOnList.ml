(** My Solution - Solved *)

let rec compress = function
  | [] -> []
  | [element] -> [element]
  | hd :: tl -> if hd = List.hd tl then compress tl
                else [hd] @ compress tl;;

(** OCaml.org Solution *)
let rec compress = function
    | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
    | smaller -> smaller;;
