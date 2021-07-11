type person =
    {first_name : string;
     surname : string;
     age : int};;

let frank =
    {first_name = "Frank";
     surname = "Smith";
     age = 40};;

let s = frank.surname;;

(**----------------------------------------------*)

type colour =
    | Red
    | Blue
    | Green
    | Yellow
    | RGB of int * int * int;;


let l = [Red; Blue; RGB (30, 255, 154)];;
