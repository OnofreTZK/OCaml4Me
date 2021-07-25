(** My Solution - Partial Solved **)
(** Failed in invalid_input case *)

exception E of string;;

let at k ls =
   match ls with
    | [] -> None
    | _ :: tl -> try Some (List.nth ls (k - 1)) with Invalid_argument _ -> None;;
    
(**| _ :: tl -> try Some (List.nth ls (k - 1)) with Invalid_argument _ -> raise (E "Failure nth");;*)

(** OCaml.org Solution *)
let rec at k = function
    | [] -> None
    | h :: t -> if k = 1 then Some h else at (k - 1) t;;
