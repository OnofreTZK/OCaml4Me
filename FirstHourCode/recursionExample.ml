let rec fib a b = 
  if a > b then []
  else if a = 0 then fib (a + 1) b
  else if a = 1 then fib (a + a) b
  else a :: fib ((a - 1) + a) b;;

let fibseq = fib 0 10;;
