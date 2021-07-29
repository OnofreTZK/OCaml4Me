(** My Solution - Solved *)

(** Improved version of rand_select that not accept repeated values *)
let rand_select lista n =
  let len = List.length lista in
  let vetor = Array.of_list lista in
  let rec aux acc count =
    let current = Array.get vetor (Random.int len) in
    if count = n then acc
    else if List.mem current acc then aux acc count 
    else aux (current :: acc) (count+1) in
  aux [] 0;;

let permutation = function
  | [] -> []
  | hd :: tl as ls -> rand_select ls (List.length ls);;

(** OCaml.org Solution *)

let rec permutation list =
    let rec extract acc n = function
      | [] -> raise Not_found
      | h :: t -> if n = 0 then (h, acc @ t) else extract (h :: acc) (n - 1) t
    in
    let extract_rand list len =
      extract [] (Random.int len) list
    in
    let rec aux acc list len =
      if len = 0 then acc else
        let picked, rest = extract_rand list len in
        aux (picked :: acc) rest (len - 1)
    in
    aux [] list (List.length list);;
