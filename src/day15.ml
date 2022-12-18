open Core

module Guy = struct
  type t =
    { sensor : Vec2.t
    ; beacon : Vec2.t
    }
  [@@deriving sexp, fields]

  let pattern =
    let open Re in
    let num = seq [ opt (char '-'); rep digit ] in
    seq
      [ str "Sensor at x="
      ; group num
      ; str ", y="
      ; group num
      ; str ": closest beacon is at x="
      ; group num
      ; str ", y="
      ; group num
      ]
    |> compile
  ;;

  let of_string s =
    let group = Re.exec pattern s in
    let sensor_x = Re.Group.get group 1 |> Int.of_string in
    let sensor_y = Re.Group.get group 2 |> Int.of_string in
    let beacon_x = Re.Group.get group 3 |> Int.of_string in
    let beacon_y = Re.Group.get group 4 |> Int.of_string in
    { sensor = sensor_x, sensor_y; beacon = beacon_x, beacon_y }
  ;;

  let manhattan { sensor; beacon } = Vec2.manhattan sensor beacon
  let contains t (p : Vec2.t) = Vec2.manhattan t.sensor p <= manhattan t
  let contains_rect t (rect : Rect.t) = List.for_all (Rect.corners rect) ~f:(contains t)
end

module Input = struct
  type t = Guy.t list [@@deriving sexp]

  let of_string s =
    let lines = String.split_lines s in
    List.map lines ~f:Guy.of_string
  ;;
end

module A = struct
  let effect (guy : Guy.t) y =
    let man = Guy.manhattan guy in
    let sensor_x, sensor_y = guy.sensor in
    let dy = abs (sensor_y - y) in
    let left = sensor_x - man in
    let right = sensor_x + man in
    let left = left + dy in
    let right = right - dy in
    left, right
  ;;

  let solve (input : Input.t) ~goal =
    let intervals = List.map input ~f:(fun guy -> effect guy goal) in
    let intervals =
      List.filter_map intervals ~f:(fun (left, right) ->
        if left <= right
        then Some (List.range ~start:`inclusive ~stop:`inclusive left right)
        else None)
    in
    let intervals = List.map intervals ~f:(fun range -> Int.Set.of_list range) in
    let hits = List.reduce_exn intervals ~f:Set.union in
    let beacons_in_hits =
      List.map input ~f:Guy.beacon
      |> List.filter_map ~f:(fun (x, y) -> if y = goal then Some x else None)
      |> Int.Set.of_list
    in
    Set.length (Set.diff hits beacons_in_hits)
  ;;

  let%expect_test _ =
    let input =
      Input.of_string
        {|Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3|}
    in
    print_s [%sexp (solve input ~goal:10 : int)];
    [%expect {| 26 |}]
  ;;

  (* let%expect_test _ =
    let input = In_channel.read_all "../input/input15.txt" |> Input.of_string in
    print_s [%sexp (solve input ~goal:2_000_000 : int)];
    [%expect {| 5176944 |}]
  ;; *)
end

module B = struct
  let solve (input : Input.t) ~bound =
    let rec search (rect : Rect.t) =
      if List.exists input ~f:(fun guy -> Guy.contains_rect guy rect)
      then None
      else if rect.x_min = rect.x_max && rect.y_min = rect.y_max
      then Some (rect.x_min, rect.y_min)
      else List.find_map (Rect.quadrants rect) ~f:search
    in
    let bounds : Rect.t = { x_min = 0; y_min = 0; x_max = bound; y_max = bound } in
    search bounds |> Option.value_exn
  ;;

  let%expect_test _ =
    let input =
      Input.of_string
        {|Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3|}
    in
    print_s [%sexp (solve input ~bound:20 : int * int)];
    [%expect {| (14 11) |}]
  ;;

  (* let%expect_test _ =
    let input = In_channel.read_all "../input/input15.txt" |> Input.of_string in
    print_s [%sexp (solve input ~bound:4_000_000 : int * int)];
    [%expect {| (3337614 2933732) |}]
  ;; *)
end

let run which =
  let input = Input.of_string In_channel.(input_all stdin) in
  match which with
  | `A ->
    let answer = A.solve input ~goal:2_000_000 in
    print_s [%sexp (answer : int)]
  | `B ->
    let answer = B.solve input ~bound:4_000_000 in
    print_s [%sexp (answer : int * int)]
;;
