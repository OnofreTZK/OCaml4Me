(** My Solution - Solved *)

let slice list i k =
  let rec aux acc count = function
    | [] -> acc
    | [element] -> if count >= i && count <= k then acc @ [element] else acc 
    | hd :: tl -> if count >= i && count <= k then [hd] @ acc @ aux acc (count+1) tl
                  else aux acc (count+1) tl in
  aux [] 0 list;;

(** OCaml.org Solution *)

let slice list i k =
    let rec take n = function
      | [] -> []
      | h :: t -> if n = 0 then [] else h :: take (n - 1) t
    in
    let rec drop n = function
      | [] -> []
      | h :: t as l -> if n = 0 then l else drop (n - 1) t
    in
    take (k - i + 1) (drop i list);;

(** This solution has a drawback, namely that the take function is not tail recursive so it may 
 * exhaust the stack when given a very long list. You may also notice that the structure of take 
 * and drop is similar and you may want to abstract their common skeleton in a single function. 
 * Here is a solution.*)

let rec fold_until f acc n = function
    | [] -> (acc, [])
    | h :: t as l -> if n = 0 then (acc, l)
                     else fold_until f (f acc h) (n - 1) t

let slice list i k =
  let _, list = fold_until (fun _ _ -> []) [] i list in
  let taken, _ = fold_until (fun acc h -> h :: acc) [] (k - i + 1) list in
  List.rev taken;;


(** Need to understand better () and _ in ocaml *)
