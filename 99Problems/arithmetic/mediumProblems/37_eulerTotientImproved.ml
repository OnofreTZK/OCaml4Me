(* My Solution - Not finished *)

let encode list =
  let rec aux count = function
    | [] -> []
    | [element] -> [(element, count)]
    | hd :: tl ->
      if hd = List.hd tl then aux (count + 1) tl
      else [(hd, count)] @ aux 1 tl in
  aux 1 list;;

let factors n =
    let rec aux acc prime n_value =
        if n_value = 1 then acc
        else if n_value mod prime = 0 then aux (prime :: acc) prime (n_value/prime)
        else aux acc (prime+1) n_value in
    encode (List.rev (aux [] 2 n));;

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a);;

let phi_improved m =
  let phi_values_list = List.map (fun (factor, frq) -> (factor-1)*(pow (frq-1) factor)) (factors m) in
  List.fold_left (fun current next -> current * next) 1 phi_values_list;;
(* val phi_improved : int -> int *)

