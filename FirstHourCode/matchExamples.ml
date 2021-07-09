let rec factorial n =
  match n with
  | 0 | 1 -> 1
  | x -> x * factorial (x - 1);;

(** Match with _ to match with anything*)

let rec factorialWith_ x =
  match x with
  | 0 | 1 -> 1
  | _ -> x * factorialWith_ (x - 1);;

(** function keyword which introduces pattern-matching*)
let rec factorialWithFunction = function
  | 0 | 1 -> 1
  | y -> y * factorialWithFunction (y - 1);;

factorial 7;;
factorialWith_ 7;;
factorialWithFunction 7;;
