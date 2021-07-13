exception E;;

exception E2 of string;;

let f a b =
  if b = 0 then raise (E2 "Division by zero") else a / b;;

f 3 0;;

print_endline "Handling exception";;

try f 10 0 with E2 _ -> 0;;
(** try calling the function
 * if match with exception E2 return 0*)

(**The other way to deal with exceptional situations in OCaml is 
 * by returning a value of a type which can represent either the correct result or an error, 
 * for example the built-in polymorphic option type, which is defined as:
 * *)

type 'a option = None | Some of 'a;;

let f2 x y =
  if y = 0 then None else Some (x / y);;
(** val f : int -> int -> int option = <fun> *)


(**We can use exception handling to build an option-style function 
 * from one which raises an exception, 
 * the built-in List.find function 
 * (which finds the first element matching a given boolean test): *)

let list_find_option p l =
    try Some (List.find p l) with
      Not_found -> None;;
