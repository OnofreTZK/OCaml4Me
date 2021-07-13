(** Create a reference to a intenger in Ocaml:*)

let r = ref 0;;

(** This reference is currently storing the integer zero. 
 * Let's put something else into it (assignment):*)

r := 100;;

(** And let's find out what the reference contains now: *)

!r;; 

(** So the := operator is used to assign to references, 
 * and the ! operator dereferences to get the contents.*)

(**We can combine multiple imperative operations with ;. 
 * For example, here is a function to swap the contents of two references of like type: *)

let swap a b =
  let t = !a in (** t receive the value referenced by a *)
  a := !b; (** a now reference b value*)
  b := t;; (** finally, b get de reference of a that was keeped by t *)

(** The return of this function is unit *)
(** Notice the function return type is unit. 
 * There is exactly one thing of type unit, and it is written (). 
 * We use unit to call a function which needs no other argument, 
 * and is only used for its imperative side effect. For example: *)

let print_number n = 
  print_string (string_of_int n);
  print_newline ();;

print_number 100;;

(** The usual imperative looping constructs are available. Here is a for loop:*)
let table n = 
  for row = 1 to n do
    for column = 1 to n do
      print_string (string_of_int ( row * column ));
      print_string " "
    done;
    print_newline ()
  done;;

let () = table 10;;

(**---------------------------------------------------------------------------------*)

let smallest_power_of_two x =
    let t = ref 1 in
      while !t < x do
        t := !t * 2
      done;
      !t;;
(** val smallest_power_of_two : int -> int = <fun> *)

(** In addition to references, the imperative part of OCaml has arrays of items of like type, 
 * whose elements can be accessed or updated in constant time: *)

let arr = [|1; 2; 3|];;

print_string (string_of_int arr.(0));;

print_newline ();;

arr.(0) <- 0;;

print_string (string_of_int arr.(0));;

print_newline ();;

(**--------------------------------------------------------------------------------*)

(** Records may have mutable fields too, which must be marked in the type: *)

type person =
  {
    first_name : string;
    surname: string; 
    mutable age : int
  };;

let birthday p =
  p.age <- p.age + 1;;



