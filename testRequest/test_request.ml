type return = {
  message : string;
  length : int
}[@@deriving yojson]

(* string Lwt.t *)
let get_body =
  let open Lwt.Syntax 
  in
  let* req = Cohttp_lwt_unix.Client.get (Uri.of_string "http://192.168.1.217:4444/blockchain/mine") 
  in
  req
  |> fun (resp, body) -> resp |> fun _ -> body 
  |> Cohttp_lwt.Body.to_string 

(* Yojson.Safe.t *)
let json = 
    Yojson.Safe.from_string (Lwt_main.run get_body)
  
let obj = 
    match [%of_yojson: return] json with
      | Ok ob -> ob
      | Error err -> Printf.printf "Error ja no json\n" |> fun () -> raise (Invalid_argument err)


let () = Printf.printf "message: %s\n%!" obj.message |> fun () -> Printf.printf "size: %d\n%!" obj.length

(*
let body =
  Client.get (Uri.of_string "http://192.168.1.217:4444/blockchain/mine") >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

let () =
  let body = Lwt_main.run body in
  print_endline ("Received body\n" ^ body)
*)
