open User_yojson

type error =
  | Database_error of string

(* Migrations-related helper functions. *)
val migrate : (module Rapper_helper.CONNECTION) -> (unit, [> Caqti_error.call_or_retrieve ]) result Lwt.t
(* Reference is the documentation *)
(* val rollback : unit -> (unit, error) result Lwt.t *)

(* Core functions *)
val get_all : unit -> UserJson.t list Lwt.t
val add : UserJson.t -> unit Lwt.t
val remove : int -> (unit, error) result Lwt.t
val clear : unit -> (unit, error) result Lwt.t

