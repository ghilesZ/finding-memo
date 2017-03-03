# finding-memo
A memoization extension syntax for ocaml.

It allows you to simply declare memoized functions as in the following example :

```ocaml
let time f x =
  let t0 = Unix.gettimeofday() in
  let res = f x in
  let t1 = Unix.gettimeofday() in
  Format.printf "Execution time: %fs\n" (t1 -. t0);
  res

(* Classic recursive definition of fibonacci function *)
let rec fibo1 = function
  | 0 | 1 -> 1
  | x -> fibo1 (x-1) + fibo1 (x-2)

(* fibonacci function using memoization *)
let%memo fibo2 = function
  | 0 | 1 -> 1
  | x -> fibo2 (x-1) + fibo2 (x-2)

let _ =
  Format.printf "%i\n%!" (time fibo1 50);
  Format.printf "%i\n%!" (time fibo2 50)
```
which outputs :

```shell
Execution time: 435.556459s
20365011074
Execution time: 0.000031s
20365011074
```
#### How it's done?
As one can expect, calling **(fibo2 50)** will register every intermediate call of (**fibo2**) thus allowing good performances.
This can't be done by using a naive memoization utilitie as :
```ocaml
let memoizator f =
  let tbl = Hashtbl.create 1000 in
  fun x ->  try Hashtbl.find tbl x
            with Not_found ->
              let res = f x in
              Hashtbl.add tbl x res;
              x
```
The trick here is to redefine a fixpoint combinator¹ and to handle the memoization inside it :
```ocaml
(* classic Y combinator *)
let rec fix f x = f (fix f) x

(* Y + memoization *)
let memofix f =
  let tbl = Hashtbl.create 1000 in
  let rec loop x =
    try Hashtbl.find tbl x
    with Not_found ->
      let res = f loop x in
      Hashtbl.add tbl x res;
      res
  in loop
```

Here we can rewrite our fibo function to make it use one of those combinator:

```ocaml
let with_fp fibo = function
  | 0 | 1 -> 1
  | x -> fibo (x-1) + fibo(x-2)

let _ =
   Format.printf "%i\n%!" ((fix with_fp) 50); (* !!! VERY SLOW !!! *)
   Format.printf "%i\n%!" ((memofix with_fp) 50)
```

Of course, no one wants to write functions that way, this why this syntax extension is needed.
It allows you to simply replace the **let rec** in your code by **let%memo** to have memoized functions.

[1] : Note that the classical definiton of a recursive function using **let rec name = body** can be seen as sugar (in fact, it's not the case) for :
```ocaml
let rec fix f x = f (fix f) x

(*equivalent to let rec name = body*)
let name =
  let recursor name = body in
  fix recursor
```

Here, we're doing quite the same thing using a memoizing fixpoint combinator :)

#### How to test?
##### Dependencies :
- ocamlbuild
- oasis

##### build :
```shell
make test
./test.native
```

### Usability
This is the first time i try to write a ppx extension. Dont rely on this code, it's just a proof of concept.