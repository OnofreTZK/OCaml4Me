open Opium
open User_yojson

(* List of users *)
(*let users = ref []*)

let print_query_params req =
  let name = Router.param req "name" in
  let username = Router.param req "username" in
  let email = Router.param req "email" in
  let password = Router.param req "password" in
  let user = {UserJson.name; username; email; password} |> UserJson.to_yojson in
  Lwt.return (Response.of_json user)

(* POST request -> Figured out the problem: The send type on postman 
 * In postman only work if i send a raw json post request *)
let create_user req =
  let open Lwt.Syntax in
  let* json = Request.to_json_exn req in
  let response = 
    match UserJson.of_yojson json with
    (*users := user :: !users;*) 
    | Ok usr -> usr |> Storage.insert_user |> fun _ -> Response.of_json (UserJson.to_yojson usr)
    | Error err -> err |> fun _ -> Response.make ~status: `Bad_request ()
  in
  Lwt.return response
;; 

(* GET request *)
let read_all_users req =
  let open Lwt.Syntax in
  let* user_list = Storage.get_users () in
  let json = [%to_yojson: UserJson.t list] user_list in
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
