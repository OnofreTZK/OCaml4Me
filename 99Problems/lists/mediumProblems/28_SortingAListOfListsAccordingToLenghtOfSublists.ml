(** OCaml.org Solution *)

(* We might not be allowed to use built-in List.sort, so here's an
     eight-line implementation of insertion sort — O(n²) time
     complexity. *)
  let rec insert cmp e = function
    | [] -> [e]
    | h :: t as l -> if cmp e h <= 0 then e :: l else h :: insert cmp e t

  let rec sort cmp = function
    | [] -> []
    | h :: t -> insert cmp h (sort cmp t)

  (* Sorting according to length : prepend length, sort, remove length *)
  let length_sort lists =
    let lists = List.map (fun list -> List.length list, list) lists in
    let lists = sort (fun a b -> compare (fst a) (fst b)) lists in
    List.map snd lists;;
val insert : ('a -> 'a -> int) -> 'a -> 'a list -> 'a list = <fun>
val sort : ('a -> 'a -> int) -> 'a list -> 'a list = <fun>
val length_sort : 'a list list -> 'a list list = <fun>
# (* Sorting according to length frequency : prepend frequency, sort,
     remove frequency. Frequencies are extracted by sorting lengths
     and applying RLE to count occurrences of each length (see problem
     "Run-length encoding of a list.") *)
  let rle list =
    let rec aux count acc = function
      | [] -> [] (* Can only be reached if original list is empty *)
      | [x] -> (x, count + 1) :: acc
      | a :: (b :: _ as t) ->
         if a = b then aux (count + 1) acc t
         else aux 0 ((a, count + 1) :: acc) t in
    aux 0 [] list

  let frequency_sort lists =
    let lengths = List.map List.length lists in
    let freq = rle (sort compare lengths) in
    let by_freq =
      List.map (fun list -> List.assoc (List.length list) freq , list) lists in
    let sorted = sort (fun a b -> compare (fst a) (fst b)) by_freq in
    List.map snd sorted;;
