open Mirage_crypto_pk

let private_key = Rsa.generate ~bits:64 ()

let public_key = Rsa.pub_of_priv private_key

(* or_digest is a type not a function -> need to know how to represent 
 * the pub key or know if i really need this *)
let str_private_key = match Rsa.or_digest private_key with
                      | `Message msg -> Printf.printf "Message(?): %s\n%!" msg |> fun () -> msg
                      | `Digest _ -> Printf.printf "What is a Cstruct??\n%!" |> fun () -> "test"

let () = Printf.printf "Final string: %s/\n%!" str_private_key 


