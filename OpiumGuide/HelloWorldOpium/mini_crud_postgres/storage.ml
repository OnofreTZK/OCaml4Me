open User_yojson
(*open Db_controller*)

let db = "database.json";;

let get_users () =
  Lwt_io.with_file ~mode:Input db (fun input_channel ->
      let open Lwt.Syntax in
      let* db_str = Lwt_io.read_lines input_channel |> Lwt_stream.to_list
      in
      let db_json =
        Yojson.Safe.from_string (String.concat "\n" db_str)
      in 
      match [%of_yojson: UserJson.t list ] (db_json) with
      | Ok users -> Lwt.return users 
      | Error err -> raise (Invalid_argument err))
;;

(*https://ocsigen.org/lwt/5.3.0/api/Lwt.Syntax*)
(* Lwt.bind is let* on Lwt.Syntax doesn't need to be manually binded*)
(* Old function read_user when eval on compile time always ends with the state of
 * database json in compile time, thats why never imanaged to get updated after a post
 *
 * Understand compile time evaluation is core to debug ocaml code
 * Functions here are citizens of first class *)

let insert_user usr =
  let open Lwt.Syntax in
  let* users = get_users () in
  let users = usr :: users in
  Lwt_io.with_file ~mode:Output db (fun output_channel ->
      let users_string =
        users |> [%to_yojson: UserJson.t list] |> Yojson.Safe.pretty_to_string
      in
      Lwt_io.write output_channel users_string);
;;
