let board_1 = [
  ['t'; 'h'; 'i'; 's'; 'i'; 's'; 'a'];
  ['s'; 'i'; 'm'; 'p'; 'l'; 'e'; 'x'];
  ['b'; 'x'; 'x'; 'x'; 'x'; 'e'; 'b'];
  ['x'; 'o'; 'g'; 'g'; 'l'; 'x'; 'o'];
  ['x'; 'x'; 'x'; 'D'; 'T'; 'r'; 'a'];
  ['R'; 'E'; 'P'; 'E'; 'A'; 'd'; 'x'];
  ['x'; 'x'; 'x'; 'x'; 'x'; 'x'; 'x'];
  ['N'; 'O'; 'T'; 'R'; 'E'; '-'; 'P'];
  ['x'; 'x'; 'D'; 'E'; 'T'; 'A'; 'E'];
]
;;

let board_2 = 
  [['y';'g';'f';'y';'e';'i'];
  ['c';'o';'r';'p';'o';'u'];
  ['j';'u';'z';'s';'e';'l'];
  ['s';'y';'u';'r';'h';'p'];
  ['e';'a';'e';'g';'n';'d'];
  ['h';'e';'l';'s';'a';'t']];
;;

(* In order to make it easy to work, I decided to convert the 2D list into a 2D array*)
let arr_board_1 = Array.of_list board_1 |> Array.map (fun ls -> Array.of_list ls);;
let arr_board_2 = Array.of_list board_2 |> Array.map (fun ls -> Array.of_list ls);;

(* Converting the graph from array to an adjacency list*)
let to_adj_list board =
  let row_l = (Array.length board) - 1
  in
  let col_l = (Array.length board.(0)) - 1
  in
  let adj = ref []
  in
  for i = 0 to row_l do 
    for j = 0 to col_l do
      let current = board.(i).(j)
      in
      let temp_ls = ref []
      in
      for k = 0 to 2 do
        for l = 0 to 2 do
          if not ((i+k-1 = -1) || (j+l-1 = -1) || (i+k-1 >= row_l+1) || (j+l-1 >= col_l+1)) then 
            let node = board.(i+k-1).(j+l-1)
            in
            if current = node then ()
            else (temp_ls := node :: !temp_ls) |> fun _ -> ()
        done
      done;
      adj := (current, !temp_ls) :: !adj
    done
  done;
  List.rev !adj
;;

let adj_list_1 = to_adj_list arr_board_1;;
let adj_list_2 = to_adj_list arr_board_2;;

(* Queue of letters *)
let queue_letters word =
    let qu = Queue.create ()
    in
    word 
    |> String.to_seq 
    |> List.of_seq 
    |> List.iter (fun letter -> Queue.push letter qu)
    |> fun () -> qu
;;

let boggle graph word =
  let letters = queue_letters word
  in
  let rec aux src dst visited =
    match Queue.is_empty letters with
    | true  -> src = dst
    | false -> if src = dst then aux (Queue.take letters) (Queue.take letters) (src :: visited)
               else if (List.mem src visited) then false
               else
                 try
                  List.map (fun neighbor ->
                                  aux neighbor dst (src :: visited)) (List.assoc src graph)
                  |> List.mem true
                 with _ ->
                   false 
  in
  let src = Queue.take letters
  in
  let dst = Queue.take letters
  in
  aux src dst []
;;

type board_number = Board_1 | Board_2

let get_board = function
  | Board_1 -> (1, adj_list_1)
  | Board_2 -> (2, adj_list_2)

let test label word flag =
  let expect =
    match flag with
    | true  -> "TRUE"
    | false -> "FALSE"
  in
  match boggle (snd (get_board label)) word with
  | true  -> Printf.printf 
                "%s in board %d is TRUE | expected to be %s\n" word (fst (get_board label)) expect 
             |> fun () -> if flag then 1 else 0
  | false -> Printf.printf 
                "%s in board %d IS FALSE | expecteded to be %s\n" word (fst (get_board label)) expect
             |> fun () -> if not (flag) then 1 else 0
;;

let suite () =
  let cases = [
    ("this", Board_1, true);
    ("not", Board_1, false);
    ("board", Board_1, true);
    ("simple", Board_1, true);
    ("REPEATED", Board_1, false);
    ("NOTRE-PEATED", Board_1, true);
    ("yours", Board_2, true);
    ("sana", Board_2, false);
    ("san", Board_2, true);
    ("danger", Board_2, true);
    ("help", Board_2, true);
    ("vomit", Board_2, false)]
  in
  let hits = List.fold_left (fun acc (word, label, flag) -> (test label word flag) + acc) 0 cases
  in
  let result = (float_of_int hits) /. (float_of_int (List.length cases))
  in
  Printf.printf "\n\n\tAccuracy: %.2f\n" result
;;



