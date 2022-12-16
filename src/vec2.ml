open Core

module T = struct
  type t = int * int [@@deriving hash, sexp, compare]
end

include T
include Hashable.Make (T)
include Comparable.Make (T)

let manhattan (x, y) (x', y') = abs (x - x') + abs (y - y')
let ( + ) (x, y) (x', y') = x + x', y + y'
let ( - ) (x, y) (x', y') = x - x', y - y'
let map (x, y) ~f = f x, f y
