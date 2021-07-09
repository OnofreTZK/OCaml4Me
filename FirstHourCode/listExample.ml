(** List built-in operators *)
(** :: or cons operator --> adds one element to the front of a list *)
1 :: [2; 3];;

(** @ or append operator combines two lists*)
[1] @ [2; 3];;

(** Function which operate over lists by pattern matching *)
let rec total l =
match l with
| [] -> 0
| h :: t -> h + total t;;
(** l = list
 * h = head of l
 * t = tail of l( l without its first element )
 * in this case the :: is used do deconstruct the list changing 
 * the head in each recursive iteraction*)

total [1; 2; 3; 4; 5];;

(**  Polymorphic function  *)

let rec length l =
  match l with
  | [] -> 0
  | _ :: t -> 1 + length t;;

(** This function operates not just on lists of integers, 
 * but on any kind of list. This is indicated by the type, 
 * which allows its input to be 'a list (alpha list)*)

(** the pattern _ :: t doesnt inspect the type of the head,
 * so the type here is not relevant *)

length [1; 2; 3; 4; 5];;


let rec append a b =
  match a with 
  | [] -> b
  | h :: t -> h :: append t b;;

(** list a is copied at each recursive iteraction
 * The current head wait for the return of the new list with 
 * each new a head added do the a[h+1] :: b *)

(** Higher order function *)

let rec map func l =
  match l with
  | [] -> [] 
  | h :: t -> func h :: map func t;;

(** Takes a function as a parameter or returns a function 
 *
 * This map function, given a function of type 'a -> 'b and a list of 'as, 
 * will build a list of 'bs. Sometimes 'a and 'b might be the same type, of course*)

 map total [[1; 2]; [3; 4]; [5; 6]];;
 map (fun x -> x * 2) [1; 2; 3];;

(** The syntax fun ... -> ... is used to build a function without a name - 
 * one we will only use in one place in the program*)


(** We need not give a function all its arguments at once. This is called partial application*)






