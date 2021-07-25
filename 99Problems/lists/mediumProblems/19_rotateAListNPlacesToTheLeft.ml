(** My Solution - Insane wrong movements i am drunk*)

let rotate list n =
  let rec aux acc count = function
    | [] -> acc
    | [element] -> if n > 0 then acc @ [element] else element :: acc
    | hd :: tl as l -> 
      if n > 0 && count < n then aux (acc @ [hd]) (count+1) tl
      else if n < 0 && count > n then aux ([(List.hd (List.rev l) )] @ acc ) (count-1) (List.tl (List.rev l))
      else if n < 0 then aux (acc @ [List.hd (List.rev l)]) (count-1) (List.tl (List.rev l))
      else aux ([hd] @ acc) (count+1) tl in
  aux [] 0 list;;

(** OCaml.org Solution *)
(** I will try with my split function later *)

let split list n =
    let rec aux i acc = function
      | [] -> List.rev acc, []
      | h :: t as l -> if i = 0 then List.rev acc, l
                       else aux (i - 1) (h :: acc) t  in
    aux n [] list

  let rotate list n =
    let len = List.length list in
    (* Compute a rotation value between 0 and len - 1 *)
    let n = if len = 0 then 0 else (n mod len + len) mod len in
    if n = 0 then list
    else let a, b = split list n in b @ a;;
