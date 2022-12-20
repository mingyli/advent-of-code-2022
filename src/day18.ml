open Core

module Input = struct
  type t = (int * int * int) list

  let of_string s =
    String.split_lines s
    |> List.map ~f:(fun line ->
         match String.split line ~on:',' with
         | [ x; y; z ] -> Int.of_string x, Int.of_string y, Int.of_string z
         | _ -> assert false)
  ;;
end

let adjacent one two = Vec3.manhattan one two = 1

let neighbors (a, b, c) =
  [ a + 1, b, c; a - 1, b, c; a, b + 1, c; a, b - 1, c; a, b, c + 1; a, b, c - 1 ]
;;

module A = struct
  let solve (input : Input.t) =
    let max = 6 * List.length input in
    let adjacents =
      List.cartesian_product input input
      |> List.count ~f:(fun (one, two) ->
           if [%equal: int * int * int] one two then false else adjacent one two)
    in
    let neighbors =
      List.fold input ~init:Vec3.Map.empty ~f:(fun n cube ->
        List.fold (neighbors cube) ~init:n ~f:(fun n cube ->
          Map.update n cube ~f:(function
            | None -> 1
            | Some c -> c + 1)))
    in
    let neighbors =
      Map.filter_keys neighbors ~f:(fun n -> not (List.exists input ~f:(Vec3.equal n)))
    in
    print_s [%message (Map.count neighbors ~f:(fun c -> c = 6) : int)];
    print_s [%message (Map.count neighbors ~f:(fun c -> c = 5) : int)];
    max - adjacents
  ;;

  let%expect_test _ =
    let input =
      Input.of_string
        {|2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5|}
    in
    print_s [%sexp (solve input : int)];
    [%expect
      {|
    ("Map.count neighbors ~f:(fun c -> c = 6)" 1)
    ("Map.count neighbors ~f:(fun c -> c = 5)" 0)
    64 |}]
  ;;

  (* let%expect_test _ =
    let input = In_channel.read_all "../input/input18.txt" |> Input.of_string in
    print_s [%sexp (solve input : int)];
    [%expect
      {|
    ("Map.count neighbors ~f:(fun c -> c = 6)" 31)
    ("Map.count neighbors ~f:(fun c -> c = 5)" 84)
    3650 |}] *)
end

module B = struct
  let out_of_bounds (x, y, z) = x < -1 || x > 22 || y < -1 || y > 22 || z < -1 || z > 22

  let solve (input : Input.t) =
    let input = Vec3.Set.of_list input in
    let visited = Vec3.Hash_set.create () in
    let xs = Set.map (module Int) input ~f:(fun (x, _, _) -> x) in
    let ys = Set.map (module Int) input ~f:(fun (_, y, _) -> y) in
    let zs = Set.map (module Int) input ~f:(fun (_, _, z) -> z) in
    print_s [%message (xs : Int.Set.t) (ys : Int.Set.t) (zs : Int.Set.t)];
    let rec dfs curr =
      if Hash_set.mem visited curr
      then 0
      else if out_of_bounds curr
      then (
        Hash_set.add visited curr;
        0)
      else (
        Hash_set.add visited curr;
        let neighbors = neighbors curr in
        List.sum
          (module Int)
          neighbors
          ~f:(fun neighbor -> if Set.mem input neighbor then 1 else dfs neighbor))
    in
    dfs (1, 1, 1)
  ;;

  let%expect_test _ =
    let input =
      Input.of_string
        {|2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5|}
    in
    print_s [%sexp (solve input : int)];
    [%expect {|
    ((xs (1 2 3)) (ys (1 2 3)) (zs (1 2 3 4 5 6)))
    58 |}]
  ;;

  let%expect_test _ =
    let input = In_channel.read_all "../input/input18.txt" |> Input.of_string in
    print_s [%sexp (solve input : int)];
    [%expect
      {|
    ((xs (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19))
     (ys (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19))
     (zs (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)))
    2118 |}]
  ;;
end
