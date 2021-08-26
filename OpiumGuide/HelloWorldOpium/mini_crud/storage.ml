(*open User

let db = "database.json"

let read_all_users =
  Lwt_io.with_file ~mode:Input db (fun input_channel ->
      let open Lwt.Syntax in
      let+ db_str = Lwt_io.read_lines input_channel |> Lwt_stream.to_list
      in
      let db_json =
        Yojson.Safe.from_string (String.concat "\n" db_str)
      in db_json) 
;;
(*
let insert_user usr =
  let open Lwt.Syntax in
  let+ users = read_all_users in
  let users = usr :: users in
  Lwt_io.with_file ~mode:Output database_file (fun output_channel ->
      let messages_string =
        messages |> [%to_yojson: message list] |> Yojson.Safe.pretty_to_string
      in
      Lwt_io.write output_channel messages_string) *)

let insert_user usr =
  Lwt.return ()
;;*)
