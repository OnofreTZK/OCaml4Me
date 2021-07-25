(** My Solution - :( *)

let split list len = 
  let rec aux acc count = function
    | [] -> acc
    | [element] -> [element] :: acc
    | hd :: tl -> if count <= len then aux (([hd] @ List.hd acc) :: acc) (count+1) tl
                  else aux (acc @ ([hd] :: List.tl acc) ) (count+1) tl in
  aux [[]] 1 list;;

(** Second Try - Solved (thanks to the wine)*) 

let split list len =
  let add_to_tuple value (list1, list2) =
    (list1, [value] @ list2) in
  let rec aux acc count = function
    | [] -> (acc, [])
    | [element] -> if count <= len then (acc @ [element], []) else (acc, [element])
    | hd :: tl -> if count <= len then aux (acc @ [hd]) (count+1) tl
                  else add_to_tuple hd (aux acc (count+1) (tl)) in
  aux [] 1 list;;


(** OCaml.org *)

let split list n =
    let rec aux i acc = function
      | [] -> List.rev acc, []
      | h :: t as l -> if i = 0 then List.rev acc, l
                       else aux (i - 1) (h :: acc)  t
    in
      aux n [] list;;

(** OCaml.org type: 'a list -> int -> 'a list * 'a list = <fun> *)
(** My type: 'a list -> int -> 'a list list = <fun> *)

