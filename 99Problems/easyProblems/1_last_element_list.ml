(** My Solution - SOLVED *)
let last_element l =
  try Some (List.nth l ((List.length l) - 1)) with Invalid_argument _ -> None;;

(** OCaml.org Solution *)
 let rec last = function
    | [] -> None
    | [x] -> Some x
    | _ :: t -> last t;;

(** I'm still thinking in the imperative way... *) 


