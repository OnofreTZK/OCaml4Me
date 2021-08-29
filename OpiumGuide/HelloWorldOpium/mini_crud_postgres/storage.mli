open User_yojson


val get_users : unit -> UserJson.t list Lwt.t

val insert_user : UserJson.t -> unit Lwt.t

