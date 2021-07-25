(** My Solution - Solved *)
let rec list_size ls =
  match ls with
  | [] -> 0
  | [element] -> 1
  | _ :: tl -> 1 + list_size tl;;

(** OCaml.org Solution *)
(* This function is tail-recursive: it uses a constant amount of
     stack memory regardless of list size. *)
  let length list =
    let rec aux n = function
      | [] -> n
      | _ :: t -> aux (n + 1) t
    in aux 0 list;;

(** Need to understand this overrided way *)

(** In the expr -> aux 0 list the value of n will always start with 0 *)

