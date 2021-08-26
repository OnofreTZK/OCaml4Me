open Opium
open User

(* List of users *)
let users = ref []

let print_query_params req =
  let name = Router.param req "name" in
  let username = Router.param req "username" in
  let email = Router.param req "email" in
  let password = Router.param req "password" in
  let user = {User.name; username; email; password} |> User.yojson_of_t in
  Lwt.return (Response.of_json user)

(* POST request -> Figured out the problem: The send type on postman 
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
  let json = User.ljson_of_list ls in
  let response = Response.of_json json in
  req |> fun _req -> Lwt.return response
;;


let _ = 
  App.empty
  |> App.post "/user/create" create_user
  |> App.get "/user/list" read_all_users 
  |> App.get "/user/test/:name/:username/:email/:password" print_query_params
  |> App.run_command 
;;
