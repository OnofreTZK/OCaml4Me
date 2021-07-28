(** My Solution - Solved *)

let rand_select lista n =
  let len = List.length lista in
  let vetor = Array.of_list lista in
  let rec aux acc count =
    if count = n then acc
    else aux (Array.get vetor (Random.int len) :: acc) (count+1) in
  aux [] 0;;

(** Repeated random numbers but the question 
 * doesn't say a thing about it so i think it's okay *)

(** OCaml.org *)

let rand_select list n =
    let rec extract acc n = function
      | [] -> raise Not_found
      | h :: t -> if n = 0 then (h, acc @ t) else extract (h :: acc) (n - 1) t
    in
    let extract_rand list len =
      extract [] (Random.int len) list
    in
    let rec aux n acc list len =
      if n = 0 then acc else
        let picked, rest = extract_rand list len in
        aux (n - 1) (picked :: acc) rest (len - 1)
    in
    let len = List.length list in
      aux (min n len) [] list len;;

