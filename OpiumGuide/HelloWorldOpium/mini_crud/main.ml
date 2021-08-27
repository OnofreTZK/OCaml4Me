open Opium
open User_yojson

(* List of users *)
let users = ref []

let print_query_params req =
  let name = Router.param req "name" in
  let username = Router.param req "username" in
  let email = Router.param req "email" in
  let password = Router.param req "password" in
  let user = {UserJson.name; username; email; password} |> UserJson.to_yojson in
  Lwt.return (Response.of_json user)

(* POST request -> Figured out the problem: The send type on postman 
 * In postman only work if i send a raw json post request *)
let ( let* ) = Lwt.bind

let create_user req =
  let* json = Request.to_json_exn req in
  let user = 
    match UserJson.of_yojson json with
    | Ok usr -> usr
    | Error err -> raise (Invalid_argument err)
  in
  let response = 
    try 
      users := user :: !users;
      Response.of_json (UserJson.to_yojson user)
    with err -> 
      err
      |> fun _ -> (Response.make ~status: `Bad_request ())
  in
  Lwt.return response (* Without 201 status yet *)
;; 

(* TODO Fix to return a json and not string *)
let read_all_users req =
  let ls = !users in
  let json = [%to_yojson: UserJson.t list] ls in
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
