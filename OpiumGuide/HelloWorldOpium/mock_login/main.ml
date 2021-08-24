open Opium

module User = struct

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
    | _ -> failwith "invalid person json"
  ;;
end


let create_user req =
  let open Lwt.Syntax in
  let+ json = Request.to_json_exn req in
  let user = User.t_of_yojson json in
  Logs.info (fun u -> u "User signed up! -> %s\n" user.User.name);
  Response.of_json ( `Assoc ["message", `String "Successful signed up"])
;;



let _ = 
  App.empty
  |> App.port 8000
  |> App.post "/user/create" create_user
  |> App.run_command 
;;
