open Core

module Input = struct
  type t = (string * int * string list) list [@@deriving sexp]

  let pattern =
    let open Re in
    seq
      [ str "Valve "
      ; group (rep alpha)
      ; str " has flow rate="
      ; group (rep digit)
      ; alt [ str "; tunnels lead to valves "; str "; tunnel leads to valve " ]
      ; group (rep any)
      ]
    |> compile
  ;;

  let of_string s =
    String.split_lines s
    |> List.map ~f:(fun line ->
         let group = Re.exec pattern line in
         let valve = Re.Group.get group 1 in
         let flow_rate = Re.Group.get group 2 |> Int.of_string in
         let tunnels = Re.Group.get group 3 |> Str.(split (regexp ", ")) in
         valve, flow_rate, tunnels)
  ;;
end

module Distances = struct
  include Grid.Make2 (Int)

  let create () = Array.init 59 ~f:(fun _ -> Array.init 59 ~f:(fun _ -> 1 lsl 10))

  let of_input input =
    let dist = create () in
    let intern valve =
      let i =
        List.find_mapi input ~f:(fun i (v, _, _) ->
          match String.(v = valve) with
          | true -> Some i
          | false -> None)
      in
      Option.value_exn i
    in
    List.iteri input ~f:(fun i _ -> set_exn dist (i, i) 0);
    List.iteri input ~f:(fun i (_, _, tunnels) ->
      List.iter tunnels ~f:(fun tunnel -> set_exn dist (i, intern tunnel) 1));
    List.iteri input ~f:(fun k _ ->
      List.iteri input ~f:(fun i _ ->
        List.iteri input ~f:(fun j _ ->
          let d = Int.min (get_exn' dist i j) (get_exn' dist i k + get_exn' dist k j) in
          set_exn dist (i, j) d)));
    dist
  ;;
end

module Ugh = struct
  module T = struct
    type t = Bitset.t * int * int [@@deriving equal, compare, hash, sexp]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let memo f =
    let m = Hashtbl.create (module T) in
    fun x y z ->
      match Hashtbl.find m (x, y, z) with
      | Some thing -> thing
      | None ->
        let result = f x y z in
        Hashtbl.add_exn m ~key:(x, y, z) ~data:result;
        result
  ;;
end

module A = struct
  let end_time = 30

  let make_dfs (input : Input.t) ~end_time =
    let flow_rate_by_valve =
      List.map input ~f:(fun (_, flow_rate, _) -> flow_rate) |> Array.of_list
    in
    let all_good_valves =
      Array.init (List.length input) ~f:Fn.id
      |> Array.filter ~f:(fun i -> Array.get flow_rate_by_valve i > 0)
    in
    let distances = Distances.of_input input in
    let rec dfs (open_valves : Bitset.t) valve mins =
      let candidates =
        Array.filter all_good_valves ~f:(fun v -> not (Bitset.is_set open_valves v))
        |> Array.filter ~f:(fun candidate ->
             Distances.get_exn' distances valve candidate < end_time - mins - 1)
      in
      let guys =
        Array.map candidates ~f:(fun candidate ->
          let dist = Distances.get_exn' distances valve candidate in
          let time_cost = dist + 1 in
          let f =
            Array.get flow_rate_by_valve candidate * (end_time - time_cost - mins)
          in
          f + dfs (Bitset.set open_valves candidate) candidate (mins + time_cost))
      in
      match Array.max_elt guys ~compare:Int.compare with
      | None -> 0
      | Some guy -> guy
    in
    let dfs = Ugh.memo dfs in
    dfs
  ;;

  let solve (input : Input.t) =
    let aa_index =
      List.find_mapi_exn input ~f:(fun i (v, _, _) ->
        match String.(v = "AA") with
        | true -> Some i
        | false -> None)
    in
    let dfs = make_dfs input ~end_time in
    dfs (Bitset.singleton aa_index) aa_index 0
  ;;

  let%expect_test _ =
    let input =
      Input.of_string
        {|Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II|}
    in
    print_s [%sexp (solve input : int)];
    [%expect {| 1651 |}]
  ;;

  (* let%expect_test _ =
    let input = In_channel.read_all "../input/input16.txt" |> Input.of_string in
    print_s [%sexp (solve input : int)];
    [%expect {| 1828 |}]
  ;; *)
end

module B = struct
  let end_time = 26

  let solve (input : Input.t) =
    let flow_rate_by_valve =
      List.map input ~f:(fun (_, flow_rate, _) -> flow_rate) |> Array.of_list
    in
    let all_good_valves =
      Array.init (List.length input) ~f:Fn.id
      |> Array.filter ~f:(fun i -> Array.get flow_rate_by_valve i > 0)
    in
    let distances = Distances.of_input input in
    let aa_index =
      List.find_mapi_exn input ~f:(fun i (v, _, _) ->
        match String.(v = "AA") with
        | true -> Some i
        | false -> None)
    in
    let dfs = A.make_dfs input ~end_time in
    let rec dfs2 (open_valves : Bitset.t) valve mins =
      let candidates =
        Array.filter all_good_valves ~f:(fun v -> not (Bitset.is_set open_valves v))
        |> Array.filter ~f:(fun candidate ->
             Distances.get_exn' distances valve candidate < end_time - mins - 1)
      in
      let guys =
        Array.map candidates ~f:(fun candidate ->
          let dist = Distances.get_exn' distances valve candidate in
          let time_cost = dist + 1 in
          let f =
            Array.get flow_rate_by_valve candidate * (end_time - time_cost - mins)
          in
          f + dfs2 (Bitset.set open_valves candidate) candidate (mins + time_cost))
      in
      let guys = Array.append guys [| dfs open_valves aa_index 0 |] in
      match Array.max_elt guys ~compare:Int.compare with
      | None -> 0
      | Some guy -> guy
    in
    (* let dfs2 = Ugh.memo dfs2 in *)
    dfs2 (Bitset.singleton aa_index) aa_index 0
  ;;

  let%expect_test _ =
    let input =
      Input.of_string
        {|Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II|}
    in
    print_s [%sexp (solve input : int)];
    [%expect {| 1707 |}]
  ;;

  (* let%expect_test _ =
    let input = In_channel.read_all "../input/input16.txt" |> Input.of_string in
    print_s [%sexp (solve input : int)];
    [%expect {| 2292 |}]
  ;; *)
end

let run which =
  let input = Input.of_string In_channel.(input_all stdin) in
  match which with
  | `A -> print_s [%sexp (A.solve input : int)]
  | `B -> print_s [%sexp (B.solve input : int)]
;;
