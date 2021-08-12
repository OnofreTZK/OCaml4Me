(* My Solution - Just laughs *)


let append_to_first ~ls ~len =
  let rec aux acc count =
    if count = len then acc
    else aux ((string_of_int 0) ^ (List.nth ls count) ^ acc) (count+1) in
  aux "" 0;;

let append_to_second ~ls ~len =
  let rec aux acc count =
    if count < 0 then acc
    else aux ((string_of_int 1) ^ (List.nth ls count) ^ acc) (count-1) in
  aux "" (len-1);;

let gray n =
  let rec aux acc n_val =
    if n_val <= 0 then ["0"]
    else if n_val = 1 then ["0"; "1"]
    else 
      let ls = aux [] (n_val-1) in
        aux ((append_to_second ~ls ~len:(List.length ls)) :: acc) (n_val-1)
        |> (@) (aux ((append_to_first ~ls ~len:(List.length ls)) :: acc) (n_val-1)) in
  aux [] n;;

(* OCaml.org Solution *)

let gray n =
    let rec gray_next_level k l =
      if k < n then
        (* This is the core part of the Gray code construction.
         * first_half is reversed and has a "0" attached to every element.
         * Second part is reversed (it must be reversed for correct gray code).
         * Every element has "1" attached to the front.*)
        let (first_half,second_half) = (* The magic is in here *)
          List.fold_left (fun (acc1,acc2) x -> (*first_half = acc1, second_half = acc2*)
              (("0" ^ x) :: acc1, ("1" ^ x) :: acc2)) ([], []) l
        in
        (* List.rev_append turns first_half around and attaches it to second_half.
         * The result is the modified first_half in correct order attached to
         * the second_half modified in reversed order.*)
        gray_next_level (k + 1) (List.rev_append first_half second_half)
      else l
    in
      gray_next_level 1 ["0"; "1"];;

