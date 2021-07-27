(** My Solution *)

let range val1 val2 =
  let expr =  if val1 > val2 then true else false in
  let rec aux acc count = 
    if expr && count = val2 then acc @ [count]
    else if not expr && count = val2 then acc @ [count]
    else if expr && count <> val2 then acc @ [count] @ aux acc (count-1)
    else if not expr && count <> val2 then acc @ [count] @ aux acc (count+1) 
    else acc in
  aux [] val1;;

(** OCaml.org Solution *)

let range a b =
    let rec aux acc high low =
      if high >= low then
        aux (high :: acc) (high - 1) low
      else acc
    in
      if a < b then aux [] b a else List.rev (aux [] a b);;

(** Interesting, i have a lot to improve... *)
