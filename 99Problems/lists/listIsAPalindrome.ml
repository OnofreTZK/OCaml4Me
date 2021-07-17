(** My Solution - Solved *)

let is_palindrome ls = 
  match ls with
  | [] -> false
  | [element] -> true
  | hd :: tl -> ls = List.rev ls;;

(** OCaml.org Solution *)

let is_palindrome list =
    list = List.rev list;;
  (* One can use either the rev function from the previous problem, or the
     built-in List.rev *)
(** so simple... *)


