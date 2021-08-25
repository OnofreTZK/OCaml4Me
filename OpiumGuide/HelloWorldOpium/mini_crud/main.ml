open Opium

(* Abstract type user *)
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
      | _ -> failwith "invalid user json"
  ;;


  let ljson_of_list list =
    let rec aux acc = function
      | [] -> acc
      | [usr] -> aux ((Yojson.Basic.to_string (yojson_of_t usr)) ^ acc) []
      | hd :: tl -> aux ((Yojson.Basic.to_string (yojson_of_t hd)) ^ acc) tl in
    aux "" list
  ;;

end

(* List of users *)
let users = ref []

let print_query_params req =
  let name = Router.param req "name" in
  let username = Router.param req "username" in
  let email = Router.param req "email" in
  let password = Router.param req "password" in
  let user = {User.name; username; email; password} |> User.yojson_of_t in
  Lwt.return (Response.of_json user)

(* POST request -> Figured out the problemi: The send type on postman 
 * In postman only work if i send a raw json post request *)
let ( let* ) = Lwt.bind

let create_user req =
  let* json = Request.to_json_exn req in
  let user = User.t_of_yojson json in
  let response = 
    try 
      users := user :: !users;
      Response.of_json (User.yojson_of_t user)
    with err -> 
      err
      |> fun _ -> (Response.make ~status: `Service_unavailable ())
  in
  Lwt.return response (* Without 201 status yet *)
;; 

(* TODO Fix to return a json and not string *)
let read_all_users req =
  let ls = !users in
  let json_str = User.ljson_of_list ls in
  let response = Response.make ~status:`OK ~body:(Body.of_string json_str) () in
  req |> fun _req -> Lwt.return response
;;


let _ = 
  App.empty
  |> App.post "/user/create" create_user
  |> App.get "/user/list" read_all_users 
  |> App.get "/user/test/:name/:username/:email/:password" print_query_params
  |> App.run_command 
;;
