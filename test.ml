let l = [1;2;3]

let time f x =
  let t0 = Unix.gettimeofday() in
  let res = f x in
  let t1 = Unix.gettimeofday() in
  Format.printf "Execution time: %fs\n" (t1 -. t0);
  res

let _ =
  let x = 300 in
  let%memo rendu = function
  | 0 -> 0
  | n ->
     List.fold_left (fun acc x ->
         let diff = n-x in
         if diff < 0 then acc
         else min acc (1 + (rendu diff))
       ) max_int l
  in
  Format.printf "%i\n\n%!" (time rendu x)
