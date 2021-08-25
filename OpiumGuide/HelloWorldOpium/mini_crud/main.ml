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

end

(* List of users *)
(*let users = ref []*)

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
      (Response.of_json (User.yojson_of_t user)) (* Ternary like *)
    with err -> 
      err
      |> fun _ -> (Response.make ~status: `Service_unavailable ())
  in
  Lwt.return response
;; 


(*
let create_user req =
  let open Lwt.Syntax in (* to await Yojson type to Yojson.Safe.t *)
  let+ json = Request.to_json_exn req in
  let user = User.t_of_yojson json in
  Response.of_json (`Assoc [ "message", `String user.User.name ])
;;
*)

(*
let create_user req =
  req
  |> Request.to_json
  |> fun _json -> Lwt.return (Response.make ~body:(Body.of_string "Received response") ())
;;
*)
(**************************************************************************************************)


let _ = 
  App.empty
  |> App.post "/user" create_user
  |> App.get "/user/test/:name/:username/:email/:password" print_query_params
  |> App.run_command 
;;
