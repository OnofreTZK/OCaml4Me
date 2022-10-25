(* Write a function paths g a b that returns all acyclic path p from node a to node b ≠ a 
   in the graph g. The function should return the list of all paths via backtracking. *)

type 'a ograph = {
    nodes: 'a list;
    edges: ('a * 'a) list
};;

exception Invalid_input of string;;

let example_graph = { 
      nodes = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k'];
      edges = ['h', 'g';  'k', 'f';  'f', 'b';  'f', 'c';  'c', 'b'] 
};;

(* The datastructures used here are far from the most efficient ones
   but allow for a straightforward implementation. *)
(* Returns all neighbors satisfying the predicate. *)
let neighbors g a pred =
  let edge l (b, c) = if b = a && pred c then c :: l
                      else if c = a && pred b then b :: l
                      else l 
  in
  List.fold_left edge [] g.edges

let rec list_path g a to_b = 
  match to_b with
  | [] -> assert false (* [to_b] contains the path to [b]. *)
  | a' :: _ ->
     if a' = a then [to_b]
     else
       let n = neighbors g a' (fun c -> not (List.mem c to_b)) 
       in
       List.concat (List.map (fun c -> list_path g a (c :: to_b)) n)

let paths g a b =
  assert (a <> b);
  list_path g a [b];;
