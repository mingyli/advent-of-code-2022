open Core

module Grid = struct
  type t = Vec2.Hash_set.t [@@deriving sexp]

  let of_string s =
    let t = Vec2.Hash_set.create () in
    let () =
      String.split_lines s
      |> List.iteri ~f:(fun r row ->
           String.to_list row
           |> List.iteri ~f:(fun c ch ->
                match ch with
                | '#' -> Hash_set.add t (r, c)
                | _ -> ()))
    in
    t
  ;;
end

let neighbors = [ 0, 1; 0, -1; 1, -1; 1, 0; 1, 1; -1, -1; -1, 0; -1, 1 ]

let neighbors_of = function
  | `North -> [ -1, -1; -1, 0; -1, 1 ]
  | `South -> [ 1, -1; 1, 0; 1, 1 ]
  | `West -> [ -1, -1; 0, -1; 1, -1 ]
  | `East -> [ -1, 1; 0, 1; 1, 1 ]
;;

let apply proposal elf =
  let proposal =
    match proposal with
    | `North -> -1, 0
    | `South -> 1, 0
    | `West -> 0, -1
    | `East -> 0, 1
  in
  Vec2.(proposal + elf)
;;

let decisions round =
  match round mod 4 with
  | 0 -> [ `North; `South; `West; `East ]
  | 1 -> [ `South; `West; `East; `North ]
  | 2 -> [ `West; `East; `North; `South ]
  | 3 -> [ `East; `North; `South; `West ]
  | _ -> assert false
;;

let solve grid =
  let iter round grid =
    let proposals = Vec2.Table.create () in
    Hash_set.iter grid ~f:(fun elf ->
      let proposal =
        match
          List.map neighbors ~f:(Vec2.( + ) elf) |> List.count ~f:(Hash_set.mem grid)
        with
        | 0 -> None
        | _ ->
          List.find (decisions round) ~f:(fun dir ->
            let neighbors = neighbors_of dir |> List.map ~f:(Vec2.( + ) elf) in
            match List.exists neighbors ~f:(Hash_set.mem grid) with
            | true -> false
            | false -> true)
      in
      match proposal with
      | None ->
        Hashtbl.update proposals elf ~f:(function
          | None -> [ elf ]
          | Some elves -> elf :: elves)
      | Some proposal ->
        let proposal = apply proposal elf in
        Hashtbl.update proposals proposal ~f:(function
          | None -> [ elf ]
          | Some elves -> elf :: elves));
    (* print_s [%message (proposals : Vec2.t List.t Vec2.Table.t)]; *)
    let grid = Vec2.Hash_set.create () in
    Hashtbl.iteri proposals ~f:(fun ~key:proposal ~data:elves ->
      match List.length elves with
      | 0 -> assert false
      | 1 -> Hash_set.add grid proposal
      | _ -> List.iter elves ~f:(fun elf -> Hash_set.add grid elf));
    grid
  in
  let equal_rounds =
    Sequence.unfold_step ~init:(0, grid) ~f:(fun (round, grid) ->
      let new_grid = iter round grid in
      if let grid = Hash_set.to_list grid in
         let new_grid = Hash_set.to_list new_grid in
         List.equal Vec2.equal grid new_grid
      then Yield (round + 1, (round + 1, new_grid))
      else Skip (round + 1, new_grid))
  in
  (* let elves = Hash_set.to_list grid in
  let height =
    let rows = List.map elves ~f:fst in
    let min = List.min_elt rows ~compare:Int.compare |> Option.value_exn in
    let max = List.max_elt rows ~compare:Int.compare |> Option.value_exn in
    max - min + 1
  in
  let width =
    let cols = List.map elves ~f:snd in
    let min = List.min_elt cols ~compare:Int.compare |> Option.value_exn in
    let max = List.max_elt cols ~compare:Int.compare |> Option.value_exn in
    max - min + 1
  in
  (width * height) - List.length elves *)
  Sequence.hd_exn equal_rounds
;;

let%expect_test _ =
  let grid = Grid.of_string {|.....
..##.
..#..
.....
..##.
.....|} in
  print_s [%sexp (solve grid : int)];
  [%expect {|
    4 |}]
;;

let%expect_test _ =
  let grid = Grid.of_string {|....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..|} in
  print_s [%sexp (solve grid : int)];
  [%expect {|
    20 |}]
;;

let%expect_test _ =
  let input = In_channel.read_all "../input/input23.txt" |> Grid.of_string in
  print_s [%sexp (solve input : int)];
  [%expect {|
    960 |}]
;;
