open Core

module T = struct
  type t = int * int [@@deriving hash, sexp, compare]
end

include T
include Hashable.Make (T)
include Comparable.Make (T)
