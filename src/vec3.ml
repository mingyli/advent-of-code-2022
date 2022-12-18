open Core

module T = struct
  type t = int * int * int [@@deriving hash, sexp, compare]
end

include T
include Hashable.Make (T)
include Comparable.Make (T)

let manhattan (x, y, z) (x', y', z') = abs (x - x') + abs (y - y') + abs (z - z')
let ( + ) (x, y, z) (x', y', z') = x + x', y + y', z + z'
let ( - ) (x, y, z) (x', y', z') = x - x', y - y', z - z'
let map (x, y, z) ~f = f x, f y, f z
