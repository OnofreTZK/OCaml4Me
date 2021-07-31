(** OCaml.org Solution *)

(* This implementation is less streamlined than the one-extraction
    version, because more work is done on the lists after each
    transform to prepend the actual items. The end result is cleaner
    in terms of code, though. *)

    let group list sizes =
      let initial = List.map (fun size -> size, []) sizes in

    (* The core of the function. Prepend accepts a list of groups,
       each with the number of items that should be added, and
       prepends the item to every group that can support it, thus
       turning [1,a ; 2,b ; 0,c] into [ [0,x::a ; 2,b ; 0,c ];
       [1,a ; 1,x::b ; 0,c]; [ 1,a ; 2,b ; 0,c ]]

       Again, in the prolog language (for which these questions are
       originally intended), this function is a whole lot simpler.  *)
    let prepend p list =
      let emit l acc = l :: acc in
      let rec aux emit acc = function
        | [] -> emit [] acc
        | (n, l) as h :: t ->
           let acc = if n > 0 then emit ((n - 1, p :: l) :: t) acc
                     else acc in
           aux (fun l acc -> emit (h :: l) acc) acc t
      in
      aux emit [] list
    in
    let rec aux = function
      | [] -> [initial]
      | h :: t -> List.concat (List.map (prepend h) (aux t))
    in
    let all = aux list in
    (* Don't forget to eliminate all group sets that have non-full
       groups *)
    let complete = List.filter (List.for_all (fun (x, _) -> x = 0)) all in
      List.map (List.map snd) complete;;