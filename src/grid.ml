open Core

module type S2 = sig
  type item [@@deriving sexp]
  type t = item array array [@@deriving sexp]

  val width : t -> int
  val height : t -> int
  val contains : t -> Vec2.t -> bool
  val get_exn : t -> Vec2.t -> item
  val get_exn' : t -> int -> int -> item
  val set_exn : t -> Vec2.t -> item -> unit
  val set_exn' : t -> int -> int -> item -> unit
end

module Make2 (Item : sig
  type t [@@deriving sexp]
end) : S2 with type item = Item.t = struct
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
  let get_exn' t r c = t.(r).(c)
  let set_exn t (r, c) item = t.(r).(c) <- item
  let set_exn' t r c item = t.(r).(c) <- item
end
