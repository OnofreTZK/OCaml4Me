(* Abstract type user *)
module User : sig
   
  type t = 
    { name : string;
      username : string;
      email : string;
      password : string
    }

  val yojson_of_t : t -> [> `Assoc of (string * [> `String of string ]) list ]
  val t_of_yojson : [> `Assoc of (string * [> `String of string ]) list ] -> t 
  val ljson_of_list : t list -> Yojson.Safe.t

end = struct
  
  type t = 
    { name : string;
      username : string;
      email : string;
      password : string
    }

  let yojson_of_t t = `Assoc [ "name", `String t.name; "username", `String t.username;
                               "email", `String t.email; "password", `String t.password ]

   let t_of_yojson yojson =
    match yojson with
      | `Assoc [ ("name", `String name); ("username", `String username); 
                 ("email", `String email); ("password", `String password) ] -> { name; username; 
                                                                                 email; password }
      | _ -> failwith "invalid user json"
  ;;


  let ljson_of_list list =
    let rec aux acc = function
      | [] ->  acc
      | [usr] -> aux (Yojson.Safe.to_string (yojson_of_t usr) ^ acc) []
      | hd :: tl -> aux (Yojson.Safe.to_string (yojson_of_t hd) ^ acc) tl in
    Yojson.Safe.from_string (aux "" list)
  ;;

end
