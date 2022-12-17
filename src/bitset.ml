open Core

type t = int [@@deriving compare, equal, hash, sexp]

let empty = 0
let singleton i = 1 lsl i
let set t i = t lor (1 lsl i)
let unset t i = t land lnot (1 lsl i)
let is_set t i = t land (1 lsl i) <> 0
let get t i = t land (1 lsl i)
let of_array = Array.fold ~init:empty ~f:(fun bitset i -> set bitset i)
let of_list = List.fold ~init:empty ~f:(fun bitset i -> set bitset i)

let fold t ~init ~f =
  let state = ref init in
  for i = 0 to 63 do
    match is_set t i with
    | true -> state := f !state i
    | false -> ()
  done;
  !state
;;

let to_array t = fold t ~init:[||] ~f:(fun array i -> Array.append array [| i |])
let to_list t = fold t ~init:[] ~f:(fun array i -> i :: array)

let filter t ~f =
  fold t ~init:t ~f:(fun bitset i ->
    match f i with
    | true -> bitset
    | false -> unset bitset i)
;;

let union t t' = t lor t'
let inter t t' = t land t'
let diff t t' = t land lnot t'

let%expect_test _ =
  let t = empty in
  print_s [%sexp (is_set t 5 : bool)];
  [%expect {| false |}];
  let t = set t 5 in
  print_s [%sexp (is_set t 5 : bool)];
  [%expect {| true |}];
  let t = of_list [ 1; 2; 3; 4 ] in
  let sum = fold t ~init:0 ~f:(fun count i -> count + i) in
  print_s [%sexp (sum : int)];
  [%expect {| 10 |}];
  let evens = filter t ~f:(fun i -> i % 2 = 0) in
  print_s [%sexp (to_list evens : int list)];
  [%expect {| (4 2) |}];
  let odds = diff t evens in
  print_s [%sexp (to_list odds : int list)];
  [%expect {| (3 1) |}]
;;
