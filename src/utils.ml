let location_info fmt (loc : Ast.location) =
  let start, _end = loc in
  let file = start.pos_fname in
  let line = start.pos_lnum in
  let char = start.pos_cnum - start.pos_bol in
  Format.fprintf fmt {|File "%s", line %d, char %d|} file line char

let counter_from n =
  let r = ref (n - 1) in
  fun () ->
    incr r;
    !r

let str_counter_from str n =
  let counter = counter_from n in
  fun () ->
    let c = counter () in
    str ^ string_of_int c
