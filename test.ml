let l = [1;2;3]

let%memo rendre = function
  | 0 -> 0
  | n -> List.fold_left (fun acc x ->
             if n - x < 0 then acc
             else min acc (1 + rendre (n-x))
           ) max_int l


let time f x =
  let t0 = Unix.gettimeofday() in
  let res = f x in
  let t1 = Unix.gettimeofday() in
  Format.printf "Execution time: %fs\n" (t1 -. t0);
  res


let _ =
  let x = 300 in
  Format.printf "%i\n\n%!" (time rendre x)
