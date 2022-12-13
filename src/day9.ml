open Core

let dir_of = function
  | "R" -> `Right
  | "U" -> `Up
  | "L" -> `Left
  | "D" -> `Down
  | _ -> failwith "bad dir"
;;

module Pos = struct
  module T = struct
    type t = int * int [@@deriving sexp, hash, compare]
  end

  include T
  include Hashable.Make (T)

  let move (x, y) = function
    | `Right -> x + 1, y
    | `Down -> x, y - 1
    | `Left -> x - 1, y
    | `Up -> x, y + 1
  ;;

  let ( + ) (x, y) (x', y') = x + x', y + y'
  let touching (x, y) (x', y') = abs (x - x') <= 1 && abs (y - y') <= 1

  let delta (x, y) (x', y') =
    let d z z' =
      match z = z' with
      | true -> 0
      | false -> (z - z') / abs (z - z')
    in
    d x x', d y y'
  ;;
end

module Rope = struct
  type t = Pos.t array [@@deriving sexp]

  let create len = Array.create ~len (0, 0)
  let length = Array.length
  let head t = t.(0)
  let tail t = t.(length t - 1)

  let move t dir =
    t.(0) <- Pos.move t.(0) dir;
    for i = 1 to length t - 1 do
      match Pos.touching t.(i - 1) t.(i) with
      | true -> ()
      | false -> t.(i) <- Pos.(t.(i) + delta t.(i - 1) t.(i))
    done
  ;;
end

let solve length lines =
  let rope = Rope.create length in
  let visited = Pos.Hash_set.of_list [ Rope.tail rope ] in
  List.iter lines ~f:(fun line ->
    match String.split line ~on:' ' with
    | [ dir; steps ] ->
      let dir = dir_of dir in
      let steps = Int.of_string steps in
      for _ = 1 to steps do
        Rope.move rope dir;
        Hash_set.add visited (Rope.tail rope)
      done;
      ()
    | _ -> failwith "bad line");
  Hash_set.length visited
;;

module A = struct
  let solve = solve 2

  let%expect_test _ =
    let lines = {|R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2|} |> String.split_lines in
    print_s [%sexp (solve lines : int)];
    [%expect {| 13 |}]
  ;;

  let%expect_test _ =
    let input = In_channel.read_all "../input/input9.txt" |> String.split_lines in
    print_s [%sexp (solve input : int)];
    [%expect {| 6243 |}]
  ;;
end

module B = struct
  let solve = solve 10

  let%expect_test _ =
    let lines = {|R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20|} |> String.split_lines in
    print_s [%sexp (solve lines : int)];
    [%expect {| 36 |}]
  ;;

  let%expect_test _ =
    let input = In_channel.read_all "../input/input9.txt" |> String.split_lines in
    print_s [%sexp (solve input : int)];
    [%expect {| 2630 |}]
  ;;
end

let run which =
  let lines = In_channel.(input_lines stdin) in
  let solve =
    match which with
    | `A -> A.solve
    | `B -> B.solve
  in
  let answer = solve lines in
  print_s [%sexp (answer : int)]
;;
