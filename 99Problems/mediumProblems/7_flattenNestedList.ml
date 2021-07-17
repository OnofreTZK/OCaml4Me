(** Nested list node *)
type 'a node =
  | One of 'a
  | Many of 'a node list;;

(** My Solution - Solved *)

let rec flatten = function
  | [] -> []
  | [One value] -> [value]
  | [Many ls] -> flatten ls
  | head :: tail -> flatten [head] @ flatten tail;;


(** OCaml.org Solution *)
(* This function traverses the list, prepending any encountered elements
    to an accumulator, which flattens the list in inverse order. It can
    then be reversed to obtain the actual flattened list. *)

  let flatten list =
    let rec aux acc = function
      | [] -> acc
      | One x :: t -> aux (x :: acc) t
      | Many l :: t -> aux (aux acc l) t in
    List.rev (aux [] list);;

(** Am i too noob or my solutions is prettier? *)

