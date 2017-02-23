let l = [1;2;3]

let optmin x y =
  match x,y with
  | None,x | x,None -> x
  | Some x, Some y -> Some (min x y)

let succ = function
  | Some x -> Some (x+1)
  | None -> Some 1

(*Change-making problem*)
let%memo change_make = function
  | 0 -> Some 0
  | n ->
     let next acc x =
       match n-x with
       | 0 -> acc
       | x when x < 0 -> None
       | x -> rendu x |> succ |> optmin acc
     in List.fold_left next None l

let print fmt = function
  | None ->  Format.printf "no solution found"
  | Some x -> Format.printf "best solution : %i" x

let time f x =
  let t0 = Unix.gettimeofday() in
  let res = f x in
  let t1 = Unix.gettimeofday() in
  Format.printf "Execution time: %fs\n" (t1 -. t0);
  res

let _ =
  let x = 300 in
  Format.printf "%a\n%!" print (time rendu x)
