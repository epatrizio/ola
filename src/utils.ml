let location_info ?(message = None) (loc : Ast.location) =
  let start, _end = loc in
  let file = start.pos_fname in
  let line = start.pos_lnum in
  let char = start.pos_cnum - start.pos_bol in
  let mess = match message with None -> "" | Some m -> m in
  Format.sprintf {|File "%s", line %d, char %d %s|} file line char mess
