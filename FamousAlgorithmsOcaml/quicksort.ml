let rec quicksort = function
    | [] -> []                                               
    | x :: xs ->
              let smaller = List.filter ((>) x) xs in        
              let bigger = List.filter ((<) x) xs in         
                quicksort smaller @ [x] @ quicksort bigger;; 

