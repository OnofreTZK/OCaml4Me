(** My Solution *)

let remove_at k list =
  let rec aux acc count = function
    | [] -> acc
    | [element] -> if count = k then acc else acc @ [element]
    | hd :: tl -> if count = k then aux acc (count+1) tl
                  else aux (acc @ [hd]) (count+1) tl in
  aux [] 0 list;;
