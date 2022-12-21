open Core

module Input = struct
  type t = int list

  let of_string s = String.split_lines s |> List.map ~f:Int.of_string
end

module Node = struct
  type t =
    { num : int
    ; mutable prev : t
    ; mutable next : t
    }
end

let make_nodes input =
  let nodes =
    List.map input ~f:(fun num -> Node.{ num; prev = Obj.magic 0; next = Obj.magic 0 })
  in
  let () =
    List.iter
      (List.zip_exn (List.drop_last_exn nodes) (List.drop nodes 1))
      ~f:(fun (left, right) ->
        left.next <- right;
        right.prev <- left)
  in
  let head = List.hd_exn nodes in
  let last = List.last_exn nodes in
  last.next <- head;
  head.prev <- last;
  nodes
;;

let print_nodes (nodes : Node.t list) =
  let node = ref (List.hd_exn nodes) in
  let length = List.length nodes in
  for _ = 1 to length do
    let num = !node.num in
    print_string [%string "%{num#Int} "];
    node := !node.next
  done;
  print_endline ""
;;

let mix (nodes : Node.t list) ~print =
  let length = List.length nodes in
  List.iter nodes ~f:(fun node ->
    node.prev.next <- node.next;
    node.next.prev <- node.prev;
    let left = ref node.prev in
    let right = ref node.next in
    let dist = node.num mod (length - 1) in
    let () =
      match dist > 0 with
      | true ->
        for _ = 1 to dist do
          left := !left.next;
          right := !right.next
        done
      | false ->
        for _ = -1 downto dist do
          left := !left.prev;
          right := !right.prev
        done
    in
    !left.next <- node;
    node.prev <- !left;
    !right.prev <- node;
    node.next <- !right;
    if print then print_nodes nodes)
;;

let rec nth (head : Node.t) n =
  match n with
  | 0 -> head
  | n -> nth head.next (n - 1)
;;

module A = struct
  let solve (input : Input.t) ~print =
    let nodes = make_nodes input in
    mix nodes ~print;
    let zero = List.find_exn nodes ~f:(fun node -> node.num = 0) in
    let thousand = nth zero 1000 in
    let two_thousand = nth zero 2000 in
    let three_thousand = nth zero 3000 in
    print_s
      [%message (thousand.num : int) (two_thousand.num : int) (three_thousand.num : int)];
    thousand.num + two_thousand.num + three_thousand.num
  ;;

  let%expect_test _ =
    let input = Input.of_string {|1
2
-3
3
-2
0
4|} in
    print_s [%sexp (solve input ~print:true : int)];
    [%expect
      {|
    1 -3 3 -2 0 4 2
    1 -3 2 3 -2 0 4
    1 2 3 -2 -3 0 4
    1 2 -2 -3 0 3 4
    1 2 -3 0 3 4 -2
    1 2 -3 0 3 4 -2
    1 2 -3 4 0 3 -2
    ((thousand.num 4) (two_thousand.num -3) (three_thousand.num 2))
    3 |}]
  ;;

  let%expect_test _ =
    let input = In_channel.read_all "../input/input20.txt" |> Input.of_string in
    print_s [%sexp (solve input ~print:false : int)];
    [%expect
      {|
    ((thousand.num -4892) (two_thousand.num -1612) (three_thousand.num 8779))
    2275 |}]
  ;;
end

module B = struct
  let decryption_key = 811589153

  let solve (input : Input.t) ~print =
    let input = List.map input ~f:(Int.( * ) decryption_key) in
    let nodes = make_nodes input in
    Fn.apply_n_times ~n:10 (fun () -> mix nodes ~print) ();
    let zero = List.find_exn nodes ~f:(fun node -> node.num = 0) in
    let thousand = nth zero 1000 in
    let two_thousand = nth zero 2000 in
    let three_thousand = nth zero 3000 in
    print_s
      [%message (thousand.num : int) (two_thousand.num : int) (three_thousand.num : int)];
    thousand.num + two_thousand.num + three_thousand.num
  ;;

  let%expect_test _ =
    let input = Input.of_string {|1
2
-3
3
-2
0
4|} in
    print_s [%sexp (solve input ~print:false : int)];
    [%expect
      {|
    ((thousand.num 811589153) (two_thousand.num 2434767459)
     (three_thousand.num -1623178306))
    1623178306 |}]
  ;;

  let%expect_test _ =
    let input = In_channel.read_all "../input/input20.txt" |> Input.of_string in
    print_s [%sexp (solve input ~print:false : int)];
    [%expect
      {|
    ((thousand.num 7055144507029) (two_thousand.num -1029906635157)
     (three_thousand.num -1934828540752))
    4090409331120 |}]
  ;;
end
