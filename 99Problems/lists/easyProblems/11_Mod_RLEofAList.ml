
type 'a rle =
  | One of 'a
  | Many of int * 'a;;

(** My Solution - Solved *)

let encode list = 
  let rec aux count = function
    | [] -> []
    | [element] -> [One element]
    | hd :: tl -> 
      if hd = List.hd tl then aux (count + 1) tl
      else if hd <> List.hd tl && count  = 1 then [One hd] @ aux 1 tl  
      else [Many (count, hd)] @ aux 1 tl in
  aux 1 list;;

(** OCaml.org Solution *)

let encode l =
    let create_tuple cnt elem =
      if cnt = 1 then One elem
      else Many (cnt, elem) in
    let rec aux count acc = function
      | [] -> []
      | [x] -> (create_tuple (count + 1) x) :: acc
      | hd :: (snd :: _ as tl) ->
          if hd = snd then aux (count + 1) acc tl
          else aux 0 ((create_tuple (count + 1) hd) :: acc) tl in
      List.rev (aux 0 [] l);;


