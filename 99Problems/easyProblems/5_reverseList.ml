(** My Solution - SOLVED *)
let rev list =
  let rec aux = function
    | [] -> []
    | [element] -> [element]
    | hd :: tl -> aux tl @ [hd] 
  in aux list;;

(** OCaml.org Solution *)
let rev list =
    let rec aux acc = function
      | [] -> acc
      | h :: t -> aux (h :: acc) t in
    aux [] list;;

(** I didn't get the acc... *)
