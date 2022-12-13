open Core

module type Grid2 = sig
  type item [@@deriving sexp]
  type t = item array array [@@deriving sexp]

  val width : t -> int
  val height : t -> int
  val contains : t -> Pos2.t -> bool
  val get_exn : t -> Pos2.t -> item
end

module Make2 (Item : sig
  type t [@@deriving sexp]
end) : Grid2 with type item = Item.t = struct
  type item = Item.t [@@deriving sexp]
  type t = item array array [@@deriving sexp]

  let height = Array.length
  let width t = Array.length t.(0)

  let contains t (r, c) =
    let height = height t in
    let width = width t in
    r >= 0 && r < height && c >= 0 && c < width
  ;;

  let get_exn t (r, c) = t.(r).(c)
end
