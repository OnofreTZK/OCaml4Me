(** My Solution - Solved *)

let drop list int =
    let rec aux acc nth count = function
        | [] -> acc
        | [element] -> if count = nth then acc else acc @ [element]
        | hd :: tl -> if count = nth then acc @ aux acc nth 1 tl
                      else  acc @ [hd] @ aux acc nth (count + 1) tl in
    aux [] int 1 list;;

(** OCaml.org Solution *)

 let drop list n =
    let rec aux i = function
      | [] -> []
      | h :: t -> if i = n then aux 1 t else h :: aux (i + 1) t  in
    aux 1 list;;
