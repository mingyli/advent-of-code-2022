open Core

module Direction = struct
  type t =
    | Left
    | Right
  [@@deriving sexp]

  let of_char = function
    | '<' -> Left
    | '>' -> Right
    | _ -> assert false
  ;;
end

module Input = struct
  type t = Direction.t list

  let of_string s = String.to_list s |> List.map ~f:Direction.of_char
end

module Rock = struct
  type t =
    { anchor : Vec2.t
    ; mask : Vec2.t list
    }
  [@@deriving sexp_of, fields]

  let minus anchor = { anchor; mask = [ 2, 0; 3, 0; 4, 0; 5, 0 ] }
  let plus anchor = { anchor; mask = [ 3, 0; 2, 1; 3, 1; 4, 1; 3, 2 ] }
  let j anchor = { anchor; mask = [ 2, 0; 3, 0; 4, 0; 4, 1; 4, 2 ] }
  let i anchor = { anchor; mask = [ 2, 0; 2, 1; 2, 2; 2, 3 ] }
  let square anchor = { anchor; mask = [ 2, 0; 3, 0; 2, 1; 3, 1 ] }
  let cycle () = Sequence.cycle_list_exn [ 0, minus; 1, plus; 2, j; 3, i; 4, square ]
  let guh t = List.map t.mask ~f:(fun p -> Vec2.(t.anchor + p))
  let down t = { t with anchor = Vec2.(t.anchor + (0, -1)) }

  let highest t =
    guh t |> List.map ~f:snd |> List.max_elt ~compare:Int.compare |> Option.value_exn
  ;;

  let in_bounds t = List.for_all (guh t) ~f:(fun (x, _y) -> x >= 0 && x < 7)
  let out_of_bounds t = not (in_bounds t)
end

let print rocks y y' =
  for y = y downto y' do
    for x = 0 to 6 do
      match
        List.exists rocks ~f:(fun rock ->
          List.exists (Rock.guh rock) ~f:(fun p -> Vec2.(p = (x, y))))
      with
      | true -> print_string "#"
      | false -> print_string "."
    done;
    print_endline ""
  done
;;

let contains grid (r : Vec2.t) =
  Queue.exists grid ~f:(fun rock ->
    List.exists (Rock.guh rock) ~f:(fun p -> Vec2.(p = r)))
;;

let collides grid rock = List.exists (Rock.guh rock) ~f:(fun p -> contains grid p)

let apply grid rock (jet : Direction.t) =
  let hypothetical =
    match jet with
    | Left -> Rock.{ rock with anchor = Vec2.(rock.anchor + (-1, 0)) }
    | Right -> Rock.{ rock with anchor = Vec2.(rock.anchor + (1, 0)) }
  in
  match collides grid hypothetical || Rock.out_of_bounds hypothetical with
  | true -> rock
  | false -> hypothetical
;;

module Detect_cycle = struct
  module T = struct
    type t = int * int * int [@@deriving sexp, hash, compare]
  end

  include T
  include Hashable.Make (T)
end

let solve (input : Input.t) ~num_rocks ~print_cycle =
  let rocks = ref (Rock.cycle ()) in
  let jets =
    let input = List.mapi input ~f:(fun i jet -> i, jet) in
    ref (Sequence.cycle_list_exn input)
  in
  let grid =
    Queue.singleton
      Rock.{ anchor = 0, 0; mask = [ 0, 0; 1, 0; 2, 0; 3, 0; 4, 0; 5, 0; 6, 0 ] }
  in
  let height = ref 0 in
  let jeti = ref 0 in
  let detect_cycle = Detect_cycle.Table.create () in
  for nrock = 1 to num_rocks do
    let (rocki, rock), rr = Sequence.next !rocks |> Option.value_exn in
    rocks := rr;
    let anchor = 0, !height + 3 + 1 in
    let rock =
      Sequence.unfold_step ~init:(rock anchor) ~f:(fun rock ->
        let (jetii, jet), jj = Sequence.next !jets |> Option.value_exn in
        jeti := jetii;
        jets := jj;
        let rock = apply grid rock jet in
        let hypothetical = Rock.down rock in
        match collides grid hypothetical with
        | true -> Yield (rock, rock)
        | false -> Skip hypothetical)
      |> Sequence.hd_exn
    in
    Queue.enqueue grid rock;
    height := Int.max !height (Rock.highest rock);
    let key = fst (Rock.anchor rock), rocki, !jeti in
    Hashtbl.update detect_cycle key ~f:(function
      | None -> nrock, !height
      | Some (existing_nrock, existing_height) ->
        let cycle_length = nrock - existing_nrock in
        let cycle_height = !height - existing_height in
        let completed_cycles = 1000000000000 / cycle_length in
        let remainder = 1000000000000 mod cycle_length in
        if print_cycle
        then
          print_s
            [%message
              "see cycle"
                (existing_nrock, existing_height : int * int)
                (cycle_length : int)
                (cycle_height : int)
                (completed_cycles : int)
                (remainder : int)];
        nrock, !height)
  done;
  !height
;;

let%expect_test _ =
  let input = Input.of_string {|>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>|} in
  print_s [%sexp (solve input ~num_rocks:2022 ~print_cycle:false : int)];
  [%expect {| 3068 |}]
;;

let%expect_test _ =
  let input =
    In_channel.read_all "../input/input17.txt" |> String.strip |> Input.of_string
  in
  print_s [%sexp (solve input ~num_rocks:1940 ~print_cycle:true : int)];
  (* I got baited by that first collision which was not a real cycle. *)
  [%expect
    {|
      ("see cycle" ("(existing_nrock, existing_height)" (11 14))
       (cycle_length 1740) (cycle_height 2769) (completed_cycles 574712643)
       (remainder 1180))
      ("see cycle" ("(existing_nrock, existing_height)" (183 290))
       (cycle_length 1745) (cycle_height 2753) (completed_cycles 573065902)
       (remainder 1010))
      ("see cycle" ("(existing_nrock, existing_height)" (184 292))
       (cycle_length 1745) (cycle_height 2753) (completed_cycles 573065902)
       (remainder 1010))
      ("see cycle" ("(existing_nrock, existing_height)" (185 294))
       (cycle_length 1745) (cycle_height 2753) (completed_cycles 573065902)
       (remainder 1010))
      ("see cycle" ("(existing_nrock, existing_height)" (186 295))
       (cycle_length 1745) (cycle_height 2753) (completed_cycles 573065902)
       (remainder 1010))
      ("see cycle" ("(existing_nrock, existing_height)" (187 298))
       (cycle_length 1745) (cycle_height 2753) (completed_cycles 573065902)
       (remainder 1010))
      ("see cycle" ("(existing_nrock, existing_height)" (188 301))
       (cycle_length 1745) (cycle_height 2753) (completed_cycles 573065902)
       (remainder 1010))
      ("see cycle" ("(existing_nrock, existing_height)" (189 301))
       (cycle_length 1745) (cycle_height 2753) (completed_cycles 573065902)
       (remainder 1010))
      ("see cycle" ("(existing_nrock, existing_height)" (190 303))
       (cycle_length 1745) (cycle_height 2753) (completed_cycles 573065902)
       (remainder 1010))
      ("see cycle" ("(existing_nrock, existing_height)" (191 303))
       (cycle_length 1745) (cycle_height 2753) (completed_cycles 573065902)
       (remainder 1010))
      ("see cycle" ("(existing_nrock, existing_height)" (192 305))
       (cycle_length 1745) (cycle_height 2753) (completed_cycles 573065902)
       (remainder 1010))
      ("see cycle" ("(existing_nrock, existing_height)" (193 308))
       (cycle_length 1745) (cycle_height 2753) (completed_cycles 573065902)
       (remainder 1010))
      ("see cycle" ("(existing_nrock, existing_height)" (194 310))
       (cycle_length 1745) (cycle_height 2753) (completed_cycles 573065902)
       (remainder 1010))
      ("see cycle" ("(existing_nrock, existing_height)" (195 310))
       (cycle_length 1745) (cycle_height 2753) (completed_cycles 573065902)
       (remainder 1010))
      3063 |}]
;;
