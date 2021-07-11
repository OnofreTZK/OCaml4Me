type 'a tree =
  | Leaf
  | Node of 'a tree * 'a * 'a tree;;
(**'a stands for all types*)
(** Node has a left all types tree, value of the node, right all types tree*)


let tree = 
  Node(Node(Leaf, 1, Leaf), 2, Node(Node(Leaf, 3, Leaf), 4, Leaf));;

(** Recursive and polymorphic functions with trees*)

let rec total tree =
  match tree with
  | Leaf -> 0
  | Node (left, x, right) -> total left + x + total right;;
(** Tree with only a leaf is a empty tree
 * else -> total of left tree + current node value + total of right tree*)

let rec flip tree =
  match tree with
  | Leaf -> Leaf
  | Node (left, x, right) -> Node ( flip right, x, flip left);;


(** OCaml is a garbage-collected language, and will free memory for data structures when they are 
 * no longer needed.*)
