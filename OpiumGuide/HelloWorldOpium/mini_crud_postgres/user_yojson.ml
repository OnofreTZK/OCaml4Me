(* Abstract type user with ppx yojson *)
module UserJson = struct
   
  type t = 
    { name : string;
      username : string;
      email : string;
      password : string
    }[@@deriving yojson]

end
  
