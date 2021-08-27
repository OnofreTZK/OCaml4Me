open User_yojson


val get_users : UserJson.t list Lwt.t

val read_users : Yojson.Safe.t Lwt.t

val insert_user : UserJson.t -> unit Lwt.t

