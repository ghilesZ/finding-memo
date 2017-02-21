module Memo = struct
  (* fixpoint combinator + memoization *)
  let ize f =
    let tbl = Hashtbl.create 1000 in
    let rec g f =
      fun x ->
      try Hashtbl.find tbl x
      with Not_found ->
        let res = f (g f) x in
        Hashtbl.add tbl x res;
        res
    in g f
end

let time f x =
  let t0 = Unix.gettimeofday() in
  let fx = f x in
  let t1 = Unix.gettimeofday() in
  Format.printf "Execution time: %fs\n" (t1 -. t0);
  fx

let l = [1;2;3]

let _ =
  Format.printf "running without memoization ...\n";
  let rec rendre = function
    | 0 -> 0
    | n -> List.fold_left (fun acc x ->
               if n - x < 0 then acc
               else min acc (1 + rendre (n-x))
             ) max_int l
  in
  let x = 30 in
  Format.printf "%i\n\n%!" (time rendre x);
  Format.printf "running with memoization\n";
  let%memo rendre x =
    match x with
    | 0 -> 0
    | n -> List.fold_left (fun acc x ->
               if n - x < 0 then acc
               else min acc (1 + rendre (n-x))
             ) max_int l
  in
  Format.printf "%i\n%!" (time rendre x)
