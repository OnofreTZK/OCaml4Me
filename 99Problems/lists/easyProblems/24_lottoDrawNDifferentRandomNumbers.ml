(** My Solution *)

let lotto_select n range =
  let rec aux acc count =
    if count = n then acc
    else aux ((Random.int range) :: acc) (count+1) in
  List.rev (aux [] 0);;

(* [range] and [rand_select] defined in problems above *)
  (*let lotto_select n m = rand_select (range 1 m) n;;*)
