(* option utilities *)
let optmin x y =
  match x,y with
  | None,a | a,None -> a
  | Some x, Some y-> Some (min x y)

let succ = function
  | Some x -> Some (x+1)
  | None -> None

(* Change-making problem*)
let change_make coins value =
  let%memo loop n =
    let next acc piece =
      match n-piece with
      | 0 -> Some 1
      | x -> if x < 0 then None
             else optmin (succ (loop x)) acc
    in
    List.fold_left next None coins
  in loop value

(* testing *)
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
  let x = 5101 in
  Format.printf "%a\n%!" print (time (change_make [2;4;3;7;8]) x)
