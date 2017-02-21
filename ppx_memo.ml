open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let error ~loc s = raise (Location.Error (Location.error ~loc ("[%memo] " ^ s)))

let check_nem = function
  | {ppat_desc = Ppat_var {txt}; ppat_loc} -> txt
  | {ppat_loc} -> error ~loc:ppat_loc "expected a nem but was a spring roll"

let extend_mapper_with_expr mapper exprf =
  {mapper with expr = exprf}

let extend_mapper_with_str_item mapper str_itemf =
  {mapper with structure_item = str_itemf}

let expr_mapper mapper argv =
  let exprf default_expr mapper = function
    | [%expr let%memo [%p? name] = [%e? body] in [%e? scope]] ->
       let nem = check_nem name in
       let variable = Exp.ident {txt=Longident.Lident nem; loc = name.ppat_loc} in
       [%expr
        let [%p name] =
          let [%p name] =
            fun [%p name] ->
            [%e body]
          in
          let tbl = Hashtbl.create 1000 in
          let rec g = fun x ->
            try Hashtbl.find tbl x
            with Not_found ->
              let res = [%e variable] (g [%e variable]) x in
              Hashtbl.add tbl x res;
              res
          in g [%e variable]
            in [%e default_expr mapper scope]
       ]
    |  x -> default_expr mapper x
  in
  extend_mapper_with_expr mapper (exprf mapper.expr)

let stritem_mapper mapper argv =
  let str_item_f defstrit mapper = function
    | [%stri let%memo [%p? name] = [%e? body] ] ->
       let nem = check_nem name in
       let variable = Exp.ident {txt=Longident.Lident nem; loc = name.ppat_loc} in
       [%stri let [%p name] =
          let [%p name] =
            fun [%p name] ->
            [%e body]
          in
          let tbl = Hashtbl.create 1000 in
          let rec g = fun x ->
            try Hashtbl.find tbl x
            with Not_found ->
              let res = [%e variable] (g [%e variable]) x in
              Hashtbl.add tbl x res;
              res
          in g [%e variable]]

       (* Format.printf "\n\n\n\n\n%!";assert false *)
    |  x -> defstrit mapper x
  in
  extend_mapper_with_str_item mapper (str_item_f mapper.structure_item)

let build_mapper mappers : string list -> Ast_mapper.mapper =
  fun x ->
  List.fold_left (fun acc newmapper ->
       (newmapper acc x)
     ) default_mapper mappers

let () =
  build_mapper [expr_mapper; stritem_mapper]
  |> register "memo"
