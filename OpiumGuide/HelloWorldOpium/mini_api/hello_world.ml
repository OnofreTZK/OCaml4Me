open Opium

(* Abstract type Person*)
module Person = struct
  type t =
    { name : string
    ; age : int
    }

  (* Serialization *)
  let yojson_of_t t = `Assoc [ "name", `String t.name; "age", `Int t.age ]

  (* Getting from json *)
  let t_of_yojson yojson =
    match yojson with
    | `Assoc [ ("name", `String name); ("age", `Int age) ] -> { name; age }
    | _ -> failwith "invalid person json"
  ;;
end


(* GET person *)
let print_person_handler req =
  let name = Router.param req "name" in (* Query param 'name' *)
  let age = Router.param req "age" |> int_of_string in (* Query param 'age' *)
  let person = { Person.name; age } |> Person.yojson_of_t in (* "instancing" person and serializing with partial application *)
  Lwt.return (Response.of_json person) (* Return the response with the person json *)
;;

(* PATCH person *)
let update_person_handler req =
  let open Lwt.Syntax in (* Lwt package -> need to study *)
  (* to_json_exn t parses the body of the request t as a JSON structure. *)
  (* If the body of the request cannot be parsed as a JSON structure, 
   * an Invalid_argument exception is raised. Use to_json to return an option instead. *)
  let+ json = Request.to_json_exn req in
  (* Deserialize the json to create a person *)
  let person = Person.t_of_yojson json in
  (* Printing log information with the person name *)
  Logs.info (fun m -> m "Received person: %s" person.Person.name);
  (* Return the response saying that the operation was successful *)
  Response.of_json (`Assoc [ "message", `String "Person saved" ])
;;

let streaming_handler req =
  (* Getting number of attributes ? *)
  let length = Body.length req.Request.body in
  let content = Body.to_stream req.Request.body in
  let body = Lwt_stream.map String.uppercase_ascii content in
  Response.make ~body:(Body.of_stream ?length body) () |> Lwt.return
;;

(* Return a response to say hello to param query 'name' *)
let print_param_handler req =
  Printf.sprintf "Hello, %s\n" (Router.param req "name")
  |> Response.of_plain_text
  |> Lwt.return
;;

let _ =
  App.empty
  |> App.post "/hello/stream" streaming_handler
  |> App.get "/hello/:name" print_param_handler
  |> App.get "/person/:name/:age" print_person_handler
  |> App.patch "/person" update_person_handler
  |> App.run_command
;;
