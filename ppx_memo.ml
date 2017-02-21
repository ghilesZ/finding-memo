open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let error ~loc s = raise (Location.Error (Location.error ~loc ("[%memo] " ^ s)))

let check_nem = function
  | {ppat_desc = Ppat_var {txt}; ppat_loc} -> txt
  | {ppat_loc} -> error ~loc:ppat_loc "expected a nem but was a spring roll"

let exp_is_rec = function
  | Pexp_let(Recursive,_,_) -> true
  | _ -> false

let extend_mapper_with_expr mapper exprf = {default_mapper with expr = exprf}

let extend_mapper argv =
  let exprf defexpr mapper expr =
    match expr with
    (* let in *)
    | {pexp_desc =
         Pexp_extension(
             {txt = "memo"},
             PStr [
                 {pstr_desc =
                    Pstr_eval (
                        {pexp_loc; pexp_desc = Pexp_let(Recursive,_,_)},
                        _)
                 }
           ])
      } -> error ~loc:pexp_loc "let binding should not be recursive and memoized"

    | [%expr let%memo [%p? name] = fun [%p? param] -> [%e? body] in [%e? scope]] ->
       let nem = check_nem name in
       let variable = Exp.ident {txt=Longident.Lident nem; loc = name.ppat_loc} in
       [%expr
        let [%p name] =
          let open Memo in
          let [%p name] =
            fun [%p name] ->
            fun [%p param] ->
            [%e body]
          in ize [%e variable]
        in [%e mapper.expr mapper scope]
       ]
    |  x -> defexpr mapper x
  in
  extend_mapper_with_expr default_mapper (exprf default_mapper.expr)


let () = register "memo" extend_mapper
