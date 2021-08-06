(* My Solution  - Almost (some entries got wrong answers)*)

let phi n = 
  let rec aux count result n_value =
    if count > n_value then result
    else if result mod count = 0 then aux (count+1) ((result)-(result/count)) n_value
    else (*n mod count = 0 then*) aux count result (n_value/count) in
  let phi_aux = aux 2 n n in
  if phi_aux > 1 then phi_aux - (phi_aux/n)
  else phi_aux;;

(* OCaml.org Solution *)
let phi n =
    let rec count_coprime acc d =
      if d < n then
        count_coprime (if coprime n d then acc + 1 else acc) (d + 1)
      else acc
    in
      if n = 1 then 1 else count_coprime 0 1;;
      
