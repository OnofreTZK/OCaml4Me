(** My Solution - Solved *)

let encode list = 
  let rec aux count = function
    | [] -> []
    | [element] -> [(count, element)]
    | hd :: tl -> 
      if hd = List.hd tl then aux (count + 1) tl
      else [(count, hd)] @ aux 1 tl in
  aux 1 list;;

(** OCaml.org First Solution *)

let encode list =
    let rec aux count acc = function
      | [] -> [] (* Can only be reached if original list is empty *)
      | [x] -> (count + 1, x) :: acc
      | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
                              else aux 0 ((count + 1, a) :: acc) t in
    List.rev (aux 0 [] list);;

(** OCaml.org Second Solution *)
(** An alternative solution, which is shorter but requires more memory, 
 * is to use the pack function( 9 - Pack consecutives of a list): *)

let encode list =
    List.map (fun l -> (List.length l, List.hd l)) (pack list);;
