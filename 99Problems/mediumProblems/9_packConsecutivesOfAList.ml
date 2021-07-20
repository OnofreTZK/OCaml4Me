(** My Solution - Partial Solver - packed max 2 consecutives *)

let rec pack = function
  | [] -> []
  | [element] -> [[element]]
  | hd :: tl ->  if hd = List.hd tl then [[hd] @ [List.hd tl]] @ pack (List.tl tl)  
                 else [[hd]] @ pack tl;;

(** My Solution on second try - Almost!! *)

let pack2 list =
  let rec aux count = function
    | [] -> []
    | [element] -> [[element]]
    | hd :: tl -> if hd = List.hd tl then [[hd] @ List.hd (aux (count + 1) tl)] @ aux (count + 1) (List.tl tl)
                  else if hd <> List.hd tl && count > 1 then [hd] :: aux 1 tl
                  else [[hd]] @ aux 1 tl in
  aux 1 list;;

(** Need to understand what exactly the cons operator do... *)

(** OCaml.org Solution *)

let pack list =
    let rec aux current acc = function
      | [] -> []    (* Can only be reached if original list is empty *)
      | [x] -> (x :: current) :: acc
      | a :: (b :: _ as t) ->
         if a = b then aux (a :: current) acc t
         else aux [] ((a :: current) :: acc) t  in
    List.rev (aux [] [] list);;

(** Only at this moment i understand that i can use parameters as local variables... *)
