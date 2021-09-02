open User_yojson

type error =
  | Database_error of string

(* Migrations-related helper functions. *)
val migrate : unit -> (unit, error) result Lwt.t
(* val rollback : unit -> (unit, error) result Lwt.t *)

(* Core functions *)
val get_all : unit -> (UserJson.t list, error) result Lwt.t
val add : UserJson.t -> (unit, error) result Lwt.t
val remove : int -> (unit, error) result Lwt.t
val clear : unit -> (unit, error) result Lwt.t

