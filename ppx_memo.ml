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

let extend_mapper argv =
  { default_mapper with
    expr = fun mapper expr ->
           match expr with
           (* let in *)
           | { pexp_desc =
                 Pexp_extension (
                     {txt = "memo"},
                     PStr [
                         {pstr_desc =
                            Pstr_eval (
                                {pexp_loc; pexp_desc = Pexp_let(Recursive,value_binding, indulet)},
                                _)
                         }
                   ])
             }
             -> error ~loc:pexp_loc "lol rec"

           (* {e with pexp_desc = Pexp_let(r, handle value_binding, mapper.expr mapper indulet)} *)
           | [%expr let%memo [%p? name] = fun [%p? param] -> [%e? corps] in [%e? indulet]] ->
              let nem = check_nem name in
              (* let variable = Pexp_ident (Longident.Lident nem) in *)
              let variable = Exp.ident {txt=Longident.Lident nem; loc = name.ppat_loc} in
              [%expr
               let [%p name] =
                 let [%p name] =
                   fun [%p name] ->
                   fun [%p param] ->
                   [%e corps]
                 in Memo.ize [%e variable]
               in [%e mapper.expr mapper indulet]
              ]
           |  x -> default_mapper.expr mapper x
  }



let () = register "memo" extend_mapper
