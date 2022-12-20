open Core

module Blueprint = struct
  type t =
    { id : int
    ; ore_robot_cost : int
    ; clay_robot_cost : int
    ; obsidian_robot_cost : int * int
    ; geode_robot_cost : int * int
    }
  [@@deriving sexp]

  let of_string s =
    match String.split s ~on:':' with
    | [ blueprint; costs ] ->
      let id =
        match String.split blueprint ~on:' ' with
        | [ "Blueprint"; id ] -> Int.of_string id
        | _ -> assert false
      in
      let ore_robot_cost, clay_robot_cost, obsidian_robot_cost, geode_robot_cost =
        match String.split costs ~on:'.' with
        | [ ore; clay; obsidian; geode; "" ] ->
          let ore_robot_cost =
            List.nth_exn (String.split ore ~on:' ') 5 |> Int.of_string
          in
          let clay_robot_cost =
            List.nth_exn (String.split clay ~on:' ') 5 |> Int.of_string
          in
          let obsidian_robot_cost =
            let l = String.split obsidian ~on:' ' in
            List.nth_exn l 5 |> Int.of_string, List.nth_exn l 8 |> Int.of_string
          in
          let geode_robot_cost =
            let l = String.split geode ~on:' ' in
            List.nth_exn l 5 |> Int.of_string, List.nth_exn l 8 |> Int.of_string
          in
          ore_robot_cost, clay_robot_cost, obsidian_robot_cost, geode_robot_cost
        | bruh -> raise_s [%message (bruh : string list)]
      in
      { id; ore_robot_cost; clay_robot_cost; obsidian_robot_cost; geode_robot_cost }
    | _ -> assert false
  ;;
end

module Input = struct
  type t = Blueprint.t list [@@deriving sexp]

  let of_string s = String.split_lines s |> List.map ~f:Blueprint.of_string
end

type guh =
  { ore_robots : int
  ; clay_robots : int
  ; obsidian_robots : int
  ; geode_robots : int
  }
[@@deriving sexp]

let add_robot guh = function
  | `Ore -> { guh with ore_robots = guh.ore_robots + 1 }
  | `Clay -> { guh with clay_robots = guh.clay_robots + 1 }
  | `Obsidian -> { guh with obsidian_robots = guh.obsidian_robots + 1 }
  | `Geode -> { guh with geode_robots = guh.geode_robots + 1 }
;;

type bruh =
  { ores : int
  ; clays : int
  ; obsidians : int
  ; geodes : int
  }
[@@deriving sexp]

let plus bruh guh =
  { ores = bruh.ores + guh.ore_robots
  ; clays = bruh.clays + guh.clay_robots
  ; obsidians = bruh.obsidians + guh.obsidian_robots
  ; geodes = bruh.geodes + guh.geode_robots
  }
;;

let universe = [| `Geode; `Obsidian; `Clay; `Ore; `None |]

let can_afford (blueprint : Blueprint.t) (bruh : bruh) = function
  | `None -> true
  | `Ore -> bruh.ores >= blueprint.ore_robot_cost
  | `Clay -> bruh.ores >= blueprint.clay_robot_cost
  | `Obsidian ->
    bruh.ores >= fst blueprint.obsidian_robot_cost
    && bruh.clays >= snd blueprint.obsidian_robot_cost
  | `Geode ->
    bruh.ores >= fst blueprint.geode_robot_cost
    && bruh.obsidians >= snd blueprint.geode_robot_cost
;;

let solve (blueprint : Blueprint.t) ~mins =
  let spend (num_resources : bruh) = function
    | `Ore -> { num_resources with ores = num_resources.ores - blueprint.ore_robot_cost }
    | `Clay ->
      { num_resources with ores = num_resources.ores - blueprint.clay_robot_cost }
    | `Obsidian ->
      { num_resources with
        ores = num_resources.ores - fst blueprint.obsidian_robot_cost
      ; clays = num_resources.clays - snd blueprint.obsidian_robot_cost
      }
    | `Geode ->
      { num_resources with
        ores = num_resources.ores - fst blueprint.geode_robot_cost
      ; obsidians = num_resources.obsidians - snd blueprint.geode_robot_cost
      }
  in
  let best = ref 0 in
  let rec dfs minute num_robots num_resources =
    best := Int.max !best num_resources.geodes;
    (* print_s
      [%message (minute : int) (num_robots : guh) (num_resources : bruh) (!best : int)]; *)
    match minute = mins with
    | true -> num_resources.geodes
    | false ->
      let optimistic =
        (* We build a geode robot every minute from now. *)
        let time_remaining = mins - minute in
        num_resources.geodes
        + (time_remaining * num_robots.geode_robots)
        + (time_remaining * (time_remaining - 1) / 2)
      in
      (* print_s [%message (optimistic : int)]; *)
      (match optimistic > !best with
       | false -> 0
       | true ->
         (* let can_afford = can_afford blueprint num_resources in *)
         let candidates =
           universe
           |> Array.filter_map ~f:(function
                | `None ->
                  let num_resources = plus num_resources num_robots in
                  Some (dfs (minute + 1) num_robots num_resources)
                | (`Ore | `Clay | `Obsidian | `Geode) as asdf ->
                  (match can_afford blueprint num_resources asdf with
                   | false -> None
                   | true ->
                     let num_resources = spend num_resources asdf in
                     let num_resources = plus num_resources num_robots in
                     Some (dfs (minute + 1) (add_robot num_robots asdf) num_resources)))
         in
         Array.max_elt candidates ~compare:Int.compare |> Option.value_exn)
  in
  dfs
    0
    { ore_robots = 1; clay_robots = 0; obsidian_robots = 0; geode_robots = 0 }
    { ores = 0; clays = 0; obsidians = 0; geodes = 0 }
;;

let solve (input : Input.t) ~mins =
  List.map input ~f:(fun blueprint -> blueprint, solve ~mins blueprint)
  |> List.map ~f:(fun (blueprint, geodes) ->
       print_s [%message (geodes : int)];
       geodes * blueprint.id)
  |> List.sum (module Int) ~f:Fn.id
;;

(* let%expect_test _ =
  let input =
    Input.of_string
      {|Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.|}
  in
  print_s [%sexp (solve ~mins:24 input : int)];
  [%expect {|
    (geodes 9)
    (geodes 12)
    33 |}]
;; *)

let run which =
  ignore which;
  let input = Input.of_string In_channel.(input_all stdin) in
  print_s [%sexp (solve input ~mins:32 : int)]
;;
