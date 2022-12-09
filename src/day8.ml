open Core

module A = struct
  let solve lines =
    let lines =
      Array.map lines ~f:(fun line ->
        String.to_array line |> Array.map ~f:String.of_char |> Array.map ~f:Int.of_string)
    in
    let height = Array.length lines in
    let width = Array.length lines.(0) in
    let all =
      List.cartesian_product (List.init height ~f:Fn.id) (List.init width ~f:Fn.id)
    in
    let is_visible r c =
      match r with
      | 0 -> true
      | r when r = height - 1 -> true
      | r ->
        (match c with
         | 0 -> true
         | c when c = width - 1 -> true
         | c ->
           let my_tree : int = lines.(r).(c) in
           let to_height (r, c) = lines.(r).(c) in
           let up = List.filter all ~f:(fun (r', c') -> r' < r && c = c') in
           let down = List.filter all ~f:(fun (r', c') -> r' > r && c = c') in
           let left = List.filter all ~f:(fun (r', c') -> r' = r && c < c') in
           let right = List.filter all ~f:(fun (r', c') -> r' = r && c > c') in
           let up = List.map up ~f:to_height in
           let down = List.map down ~f:to_height in
           let left = List.map left ~f:to_height in
           let right = List.map right ~f:to_height in
           let good heights = List.for_all heights ~f:(fun i -> i < my_tree) in
           good up || good down || good left || good right)
    in
    List.count all ~f:(fun (r, c) -> is_visible r c)
  ;;

  let%expect_test _ =
    let lines =
      {|30373
25512
65332
33549
35390|} |> String.split_lines |> List.to_array
    in
    print_s [%sexp (solve lines : int)];
    [%expect {| 21 |}]
  ;;
end

module B = struct
  let solve lines =
    let lines =
      Array.map lines ~f:(fun line ->
        String.to_array line |> Array.map ~f:String.of_char |> Array.map ~f:Int.of_string)
    in
    let height = Array.length lines in
    let width = Array.length lines.(0) in
    let all =
      List.cartesian_product (List.init height ~f:Fn.id) (List.init width ~f:Fn.id)
    in
    let dirs = [ 1, 0; -1, 0; 0, 1; 0, -1 ] in
    let scenic r c =
      let my_tree : int = lines.(r).(c) in
      List.map dirs ~f:(fun (dr, dc) ->
        let rec aux r c =
          if r < 0 || r > height - 1 || c < 0 || c > width - 1
          then 0
          else (
            let i = lines.(r).(c) in
            match Ordering.of_int (Int.compare i my_tree) with
            | Greater | Equal -> 1
            | Less -> 1 + aux (r + dr) (c + dc))
        in
        aux (r + dr) (c + dc))
      |> List.reduce_exn ~f:Int.( * )
    in
    List.map all ~f:(fun (r, c) -> scenic r c)
    |> List.max_elt ~compare:Int.compare
    |> Option.value_exn
  ;;

  let%expect_test _ =
    let lines =
      {|30373
25512
65332
33549
35390|} |> String.split_lines |> List.to_array
    in
    print_s [%sexp (solve lines : int)];
    [%expect {| 8 |}]
  ;;
end

let run which =
  let lines = In_channel.(input_lines stdin) |> Array.of_list in
  let solve =
    match which with
    | `A -> A.solve
    | `B -> B.solve
  in
  let answer = solve lines in
  print_s [%sexp (answer : int)]
;;
