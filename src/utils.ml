(* Utils toolbox *)

(* analyzes all list items with func *)
(* 'a list -> ('a -> 'b -> 'c * 'b) -> 'b -> 'c list * 'b *)
let analyse_list list func env =
  List.fold_left
    (fun (l, env) item ->
      let item, env = func item env in
      (l @ [ item ], env) )
    ([], env) list

(* checks all list items with a boolean func *)
(* 'a list -> ('a -> bool) -> bool *)
let check_list list func =
  List.fold_left (fun acc item -> func item || acc) false list

(* [1,2,3,4,5] 3 -> [1,2,3] [4,5] *)
(* 'a list -> int -> 'a list * 'a list *)
let rec cut_list_at list n =
  assert (n >= 0);
  if n = 0 then ([], list)
  else
    match list with
    | [] -> ([], [])
    | hd :: tl ->
      let l1, l2 = cut_list_at tl (n - 1) in
      (hd :: l1, l2)
