exception Stdlib_error of string

exception Stdlib_typing_error of string

let error message = raise (Stdlib_error message)

let typing_error message = raise (Stdlib_typing_error message)

let float_of_string str =
  match float_of_string_opt str with
  | Some f -> f
  | None ->
    error
      (Format.sprintf "math stdlib, string '%s' has no float representation" str)
