
type 'a rle =
  | One of 'a
  | Many of int * 'a;;

(** My Solution - Solved *)

let decode list =
  let rec create_list count element =
    if count = 1 then [element]
    else [element] @ create_list (count - 1) element in
  let rec aux = function
    | [] -> []
    | [One value] -> [value]
    | [Many(cnt, value)] -> create_list cnt value
    | (One value) :: tl -> [value] @ aux tl
    | (Many(cnt, value)) :: tl -> create_list cnt value @ aux tl in
  aux list;;

(** OCaml.org Solution *)

let decode list =
    let rec many acc n x =
      if n = 0 then acc else many (x :: acc) (n - 1) x
    in
    let rec aux acc = function
      | [] -> acc
      | One x :: t -> aux (x :: acc) t
      | Many (n, x) :: t -> aux (many acc n x) t
    in
      aux [] (List.rev list);;
    
