open User_yojson

let db = "database.json"

let get_users =
  Lwt_io.with_file ~mode:Input db (fun input_channel ->
      let open Lwt.Syntax in
      let+ db_str = Lwt_io.read_lines input_channel |> Lwt_stream.to_list
      in
      let db_json =
        Yojson.Safe.from_string (String.concat "\n" db_str)
      in 
      match [%of_yojson: UserJson.t list ] (db_json) with
      | Ok users -> users 
      | Error err -> raise (Invalid_argument err))
;;

let read_users =
  let open Lwt.Syntax in
  let+ users = get_users in
  [%to_yojson: UserJson.t list] users
;;

let insert_user usr =
  let (let*) = Lwt.bind in
  let* users = get_users in
  let users = usr :: users in
  Lwt_io.with_file ~mode:Output db (fun output_channel ->
      let users_string =
        users |> [%to_yojson: UserJson.t list] |> Yojson.Safe.pretty_to_string
      in
      Lwt_io.write output_channel users_string);
;;
