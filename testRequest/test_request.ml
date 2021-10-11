type return = {
  message : string;
  size : int
}[@@deriving yojson]

(* string Lwt.t *)
let body = 
  let open Lwt.Syntax 
  in
  let* req = Cohttp_lwt_unix.Client.get (Uri.of_string "http://192.168.1.217:4444/blockchain/mine") 
  in
  req 
   |> fun (resp, body) -> resp 
   |> fun _ -> Cohttp_lwt.Body.to_string body 
   |> fun lwt_str -> match Lwt.state lwt_str with 
                      | Return str -> Lwt.return str 
                      | Fail err -> raise err
                      | Sleep -> Lwt.return "{message: parse failed, length: 0}"

(* Yojson.Safe.t *)
let json = 
    let open Lwt.Syntax 
    in
    let* str_body = body 
    in
    Yojson.Safe.from_string str_body
  
let obj = 
    match [%of_yojson: return] json with
      | Ok ob -> ob
      | Error err -> raise (Invalid_argument err)


let () = Printf.printf "message: %s\n%!" obj.message |> fun () -> Printf.printf "size: %d\n%!" obj.size

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
