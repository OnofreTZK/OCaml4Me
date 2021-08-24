open Opium
(* Response type with text data = Hello World and return a Lwt[Like future or promise]*)
let hello _req = Response.of_plain_text "Hello World" |> Lwt.return

(* First route /greet and what happens when comes to there*)
let greet req =
  let name = Router.param req "name" in
  Printf.sprintf "Hello, %s" name |> Response.of_plain_text |> Lwt.return

(* Initi server with routes and their respectives functions*)
let () =
  (*let open App in*)
  App.empty
  |> App.get "/" hello
  |> App.get "/greet/:name" greet
  |> App.run_command
  |> ignore
