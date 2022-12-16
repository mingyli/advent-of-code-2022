open Core

type t =
  { x_min : int
  ; x_max : int
  ; y_min : int
  ; y_max : int
  }
[@@deriving sexp, compare]

let corners { x_min; x_max; y_min; y_max } =
  [ x_min, y_min; x_min, y_max; x_max, y_min; x_max, y_max ]
;;

let quadrants t =
  let mx = (t.x_min + t.x_max) / 2 in
  let my = (t.y_min + t.y_max) / 2 in
  match t.x_min = t.x_max, t.y_min = t.y_max with
  | true, true -> [ t ]
  | true, false ->
    [ { x_min = t.x_min; x_max = t.x_max; y_min = t.y_min; y_max = my }
    ; { x_min = t.x_min; x_max = t.x_max; y_min = my + 1; y_max = t.y_max }
    ]
  | false, true ->
    [ { x_min = t.x_min; x_max = mx; y_min = t.y_min; y_max = t.y_min }
    ; { x_min = mx + 1; x_max = t.x_max; y_min = t.y_min; y_max = t.y_min }
    ]
  | false, false ->
    [ { x_min = t.x_min; x_max = mx; y_min = t.y_min; y_max = my }
    ; { x_min = mx + 1; x_max = t.x_max; y_min = t.y_min; y_max = my }
    ; { x_min = t.x_min; x_max = mx; y_min = my + 1; y_max = t.y_max }
    ; { x_min = mx + 1; x_max = t.x_max; y_min = my + 1; y_max = t.y_max }
    ]
;;
