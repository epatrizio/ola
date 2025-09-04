(* Utils toolbox *)

(* analyzes all list items with func *)
(* 'a list -> ('a -> 'b -> 'c * 'b) -> 'b -> 'c list * 'b *)
let analyse_list list func env =
  List.fold_left
    (fun (l, env) item ->
      let item, env = func item env in
      (l @ [ item ], env) )
    ([], env) list
