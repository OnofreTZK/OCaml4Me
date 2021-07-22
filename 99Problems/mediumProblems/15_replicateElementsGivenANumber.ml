(** My Solution - Solved *)

let replicate list int =
  let rec create_replicate_list value count =
    if count = 0 then []
    else if count = 1 then [value]
    else [value] @ create_replicate_list value (count - 1) in
  let rec aux acc current replicator = 
    match current with 
    | [] -> acc
    | [element] -> (create_replicate_list element replicator) @ acc
    | hd :: tl -> (create_replicate_list hd replicator) @ aux acc tl replicator in
  aux [] list int;;

(** OCaml.org Solution *)

let replicate list n =
    let rec prepend n acc x =
      if n = 0 then acc else prepend (n-1) (x :: acc) x in
    let rec aux acc = function
      | [] -> acc
      | h :: t -> aux (prepend n acc h) t in
    (* This could also be written as:
       List.fold_left (prepend n) [] (List.rev list) *)
    aux [] (List.rev list);;

(** Note that List.rev list is needed only because we want aux to be tail recursive. *)
