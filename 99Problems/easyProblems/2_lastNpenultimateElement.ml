(** My Solution - SOLVED *)
let rec last_two = function
  | [] -> None
  | [element1] -> None
  | [element1; element2] -> Some (element1, element2)
  | _ :: tl -> last_two tl;;

(** OCaml.org Solution *)
let rec last_two = function
    | [] | [_] -> None
    | [x; y] -> Some (x,y)
    | _ :: t -> last_two t;;

