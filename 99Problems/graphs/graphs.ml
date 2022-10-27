(* A graph is defined as a set of nodes and a set of edges, where each edge is a pair of 
   different nodes.

There are several ways to represent graphs in OCaml.

One method is to list all edges, an edge being a pair of nodes. 
In this form, the graph depicted opposite is represented as the following expression:*)

let edges_clause = ['h', 'g';  'k', 'f';  'f', 'b';  'f', 'c';  'c', 'b'];;

(*We call this edge-clause form. Obviously, isolated nodes cannot be represented.*)

module Graph : sig
   (* Another method is to represent the whole graph as one data object. 
      According to the definition of the graph as a pair of two sets (nodes and edges), 
      we may use the following OCaml type:
    *)
   type form = Edge_clause | Graph_term | Adjacency_list

   type 'a edge_clause = 'a * 'a list

   type 'a graph_form = {
      nodes : 'a list;
      edges : ('a * 'a) list
   }

   type 'a adjacency_list = ('a * 'a list) list 

   val neighbors : 'a graph_form -> 'a -> ('a -> bool) -> 'a list

   val list_path : 'a graph_form -> 'a -> 'a list -> 'a list list

   val paths : 'a graph_form -> 'a -> 'a -> 'a list list

   val cycles : 'a graph_form -> 'a -> 'a list list

   val depth_first_print : char adjacency_list -> char -> unit
   
   val depth_first_print_rec : char adjacency_list -> char -> unit

   val breadth_first_print : char adjacency_list -> char -> unit

   val has_path_dfs : char adjacency_list -> char -> char -> bool
   
   val has_path_bfs : char adjacency_list -> char -> char -> bool

   val undirected_path : char adjacency_list -> char -> char -> bool

   val shortest_path : char adjacency_list -> char -> char -> int 

end = struct
   
   type form = Edge_clause | Graph_term | Adjacency_list
   
   type 'a edge_clause = 'a * 'a list

   type 'a graph_form = {
      nodes : 'a list;
      edges : ('a * 'a) list
   }
   
   type 'a adjacency_list = ('a * 'a list) list 

   (* The datastructures used here are far from the most efficient ones
      but allow for a straightforward implementation. *)
   (* Returns all neighbors satisfying the predicate. *)
   (* neighbors == adjacents *)
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

   let cycles g n =
      let adjacents = neighbors g n (fun _ -> true)
      in
      let path = List.concat (List.map (fun v -> list_path g n [v]) adjacents)
      in
      List.map (fun p -> p @ [n]) path
   ;;

   let depth_first_print g n =
      let stack : char Stack.t = Stack.create () 
      in
      let () = Stack.push n stack
      in
      let rec aux st =
         match Stack.is_empty st with
         | true  -> () 
         | false -> let current = Stack.pop st
                    in
                    List.iter (fun neighbor -> 
                       Stack.push neighbor st) (List.assoc current g);
                    Printf.printf " %c " current |> fun () -> aux st
      in
      aux stack
   ;;
   
   let depth_first_print_rec g n =
      let rec aux g n' =
         Printf.printf " %c " n';
         List.iter (fun neighbor -> aux g neighbor) (List.assoc n' g)
      in
      aux g n
   ;;

   let breadth_first_print g n = 
      let queue : char Queue.t = Queue.create ()
      in
      let () = Queue.push n queue
      in
      let rec aux qu =
         match Queue.is_empty qu with
         | true  -> ()
         | false -> let current = Queue.take qu
                    in
                    List.iter (fun neighbor -> Queue.push neighbor qu) (List.assoc current g);
                    Printf.printf " %c " current |> fun () -> aux qu
      in
      aux queue
   ;;

   let rec has_path_dfs g src dst =
      if src = dst then true
      else 
         List.map 
            (fun neighbor -> has_path_dfs g neighbor dst) 
            (List.assoc src g)
         |> List.mem true
         

   let has_path_bfs g src dst =
      let queue : char Queue.t = Queue.create ()
      in
      let () = Queue.push src queue
      in
      let rec aux qu =
         match Queue.is_empty qu with
         | true  -> false
         | false -> let current = Queue.take qu
                    in
                    if current = dst then true
                    else List.iter (fun neighbor -> 
                                          Queue.push neighbor qu) (List.assoc current g)
                         |> fun () -> aux qu
      in
      aux queue
   ;;

   let undirected_path (g : char adjacency_list) (src : char) (dst : char) =
      let rec aux current visited =
         if current = dst then true
         else if List.mem current visited then false
         else List.map (fun neighbor -> 
                            aux neighbor (current :: visited)) (List.assoc current g)
              |> List.mem true
      in
      aux src []
   ;;

   let shortest_path g src dst =
       let queue : (char * int) Queue.t = Queue.create ()
       in
       let () = Queue.push (src, 0) queue
       in
       let rec aux acc qu =
          match Queue.is_empty qu with
          | true  -> acc
          | false -> let (node, distance) = Queue.take qu
                     in
                     if node = dst then distance
                     else List.iter (fun neighbor -> 
                                    Queue.push (neighbor, distance+1) qu) (List.assoc node g)
                          |> fun () -> aux (acc+1) qu
      in
      aux 0 queue
   ;;

end
;;

open Graph

let example_graph_1 : char graph_form = {
   nodes = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k'];
   edges = ['h', 'g';  'k', 'f';  'f', 'b';  'f', 'c';  'c', 'b'] 
   }
;;
(* We call this graph-term form. Note, that the lists are kept sorted, they are really sets, 
   without duplicated elements. Each edge appears only once in the edge list; 
   i.e. an edge from a node x to another node y is represented as (x,y), 
   the couple (y,x) is not present. The graph-term form is our default representation. 
   You may want to define a similar type using sets instead of lists.*)

let example_graph_2 : char graph_form = { 
   nodes = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'];
   edges = [('a', 'b'); ('a', 'd'); ('b', 'c'); ('b', 'e');
               ('c', 'e'); ('d', 'e'); ('d', 'f'); ('d', 'g');
               ('e', 'h'); ('f', 'g'); ('g', 'h')] 
   }
;;

let example_graph_3 : char graph_form = {
   nodes = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'];
   edges = [('a', 'c'); ('a', 'b'); ('c', 'e'); ('e', 'b'); ('b', 'd'); ('f', 'd')]
   }
;;

let example_graph_4 : char adjacency_list = [
   ('a', ['c'; 'b']);
   ('b', ['d']);
   ('c', ['e']);
   ('d', ['f']);
   ('e', []);
   ('f', [])
   ]

let example_graph_5 : char adjacency_list = [
   ('f', ['g'; 'i']);
   ('g', ['h']);
   ('h', []);
   ('i', ['g'; 'k']);
   ('j', ['i']);
   ('k', []);
   ]

let example_graph_6 : char adjacency_list = [
   ('i', ['j'; 'k']);
   ('j', ['i']);
   ('k', ['i'; 'm'; 'l']);
   ('m', ['k']);
   ('l', ['k']);
   ('o', ['n']);
   ('n', ['o']);
 ]

let example_graph_7 : char adjacency_list = [
   ('v', ['w'; 'z']);
   ('w', ['x'; 'v']);
   ('x', ['y'; 'w']);
   ('y', ['z'; 'x']);
   ('z', ['v'; 'y']);
 ]
