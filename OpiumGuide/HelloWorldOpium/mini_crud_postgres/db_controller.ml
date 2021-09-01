(*open User_yojson*)

(* Postgres port address *)
let connection_url = "postgresql://localhost:5432";;

type error =
  | Database_error of string

(* This is the connection pool we will use for executing DB operations. *)
let pool =
  (* Result patern (type t, err) *)
  match Caqti_lwt.connect_pool ~max_size:10 (Uri.of_string connection_url) with
  | Ok pool -> pool
  | Error err -> failwith (Caqti_error.show err)

(*
type error =
  | Database_error of string
*)

(* Helper method to map Caqti errors to our own error type. 
   val or_error : ('a, [> Caqti_error.t ]) result Lwt.t -> ('a, error) result Lwt.t *)
let or_error m =
  match%lwt m with
  | Ok a -> Ok a |> Lwt.return
  | Error e -> Error (Database_error (Caqti_error.show e)) |> Lwt.return

(* Queries *)
(* Create table request *)
let migrate_query =
  Caqti_request.exec
    Caqti_type.unit
    {| CREATE TABLE users (
          id SERIAL NOT NULL PRIMARY KEY,
          name VARCHAR,
          username VARCHAR,
          email VARCHAR,
          password VARCHAR
       )
    |}

(* Exec migration *)
let migrate () =
  let migrate' (module C : Caqti_lwt.CONNECTION) =
    C.exec migrate_query ()
  in
  Caqti_lwt.Pool.use migrate' pool |> or_error

(* Drop table request*)
let rollback_query =
  Caqti_request.exec
    Caqti_type.unit
    "DROP TABLE users"

(* Exec dropping *)
let rollback () =
  let rollback' (module C : Caqti_lwt.CONNECTION) =
    C.exec rollback_query ()
  in
  Caqti_lwt.Pool.use rollback' pool |> or_error


(* Stub these out for now. *)
let get_all () = failwith "Not implemented"
let add _content = failwith "Not implemented"
let remove _id = failwith "Not implemented"
let clear () = failwith "Not implemented"
