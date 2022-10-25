(* A graph is defined as a set of nodes and a set of edges, where each edge is a pair of 
   different nodes.

There are several ways to represent graphs in OCaml.

One method is to list all edges, an edge being a pair of nodes. 
In this form, the graph depicted opposite is represented as the following expression:*)

let edges = ['h', 'g';  'k', 'f';  'f', 'b';  'f', 'c';  'c', 'b'];;

(*We call this edge-clause form. Obviously, isolated nodes cannot be represented.*)

(* Another method is to represent the whole graph as one data object. 
   According to the definition of the graph as a pair of two sets (nodes and edges), 
   we may use the following OCaml type:
 *)

type 'a ograph = {
    nodes: 'a list;
    edges: ('a * 'a) list
};;

let example_graph = { 
      nodes = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k'];
      edges = ['h', 'g';  'k', 'f';  'f', 'b';  'f', 'c';  'c', 'b'] 
};;

(* We call this graph-term form. Note, that the lists are kept sorted, they are really sets, 
   without duplicated elements. Each edge appears only once in the edge list; 
   i.e. an edge from a node x to another node y is represented as (x,y), 
   the couple (y,x) is not present. The graph-term form is our default representation. 
   You may want to define a similar type using sets instead of lists.*)
