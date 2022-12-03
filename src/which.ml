open Core

type t =
  | A
  | B
[@@deriving enumerate, sexp]

let arg_type = Command.Arg_type.of_alist_exn [ "a", A; "A", A; "b", B; "B", B ]
