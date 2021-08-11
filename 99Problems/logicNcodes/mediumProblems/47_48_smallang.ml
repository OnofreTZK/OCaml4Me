type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr;;

(* (a v b) ^ (a ^ b) *)

And (Or (Var "a", Var "b"), And (Var "a", Var "b"));;


(* Evaluates the expression according to the values of a and b *)
 let rec eval2 a val_a b val_b = function (* Match pattern for expr *)
    | Var x -> if x = a then val_a
               else if x = b then val_b
               else failwith "The expression contains an invalid variable"
    | Not e -> not (eval2 a val_a b val_b e) (* Treating the rest of expression *)
    | And(e1, e2) -> eval2 a val_a b val_b e1 && eval2 a val_a b val_b e2 (* got it! *)
    | Or(e1, e2) -> eval2 a val_a b val_b e1 || eval2 a val_a b val_b e2;;

(* No recursion needed, just match each part of the expression *)
let table2 a b expr =
    [(true,  true,  eval2 a true  b true  expr);
     (true,  false, eval2 a true  b false expr);
     (false, true,  eval2 a false b true  expr);
     (false, false, eval2 a false b false expr)];;

(* [val_vars] is an associative list containing the truth value of
     each variable.  For efficiency, a Map or a Hashtlb(At least a got a hit here) should be
     preferred. *) (* Hashtlb to generate all the possibilities *)
  
let rec eval val_vars = function
  | Var x -> List.assoc x val_vars
  | Not e -> not (eval val_vars e)
  | And(e1, e2) -> eval val_vars e1 && eval val_vars e2
  | Or(e1, e2) -> eval val_vars e1 || eval val_vars e2

(* Again, this is an easy and short implementation rather than an
   efficient one. *)
let rec table_make val_vars vars expr =
  match vars with
  | [] -> [(List.rev val_vars, eval val_vars expr)]
  | v :: tl ->
       table_make ((v, true) :: val_vars) tl expr
     @ table_make ((v, false) :: val_vars) tl expr

let table vars expr = table_make [] vars expr;;
