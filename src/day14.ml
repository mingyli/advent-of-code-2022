open Core

module Cell = struct
  type t =
    | Air
    | Rock
    | Source
    | Sand
  [@@deriving sexp]

  let to_string = function
    | Air -> " "
    | Rock -> "#"
    | Source -> "+"
    | Sand -> "o"
  ;;
end

module Grid = Grid.Make2 (Cell)

module Input = struct
  let source_col = 500
  let source_pos = 0, 500
  let cave_floor_dist = 2

  type t =
    { max_r : int
    ; min_c : int
    ; max_c : int
    ; grid : Grid.t
    }
  [@@deriving sexp]

  let to_string t =
    Array.map t.grid ~f:(fun row ->
      List.range ~start:`inclusive ~stop:`inclusive t.min_c t.max_c
      |> List.map ~f:(Array.get row)
      |> List.map ~f:Cell.to_string
      |> String.concat)
    |> String.concat_array ~sep:"\n"
  ;;

  let vec2_of_string s =
    match String.split s ~on:',' with
    | [ c; r ] -> Int.of_string r, Int.of_string c
    | _ -> assert false
  ;;

  let delta p p' =
    let d = Vec2.(p - p') in
    Vec2.map d ~f:(fun i -> Int.sign i |> Base.Sign.to_int)
  ;;

  let of_string s =
    let lines = String.split_lines s in
    let lines =
      List.map lines ~f:(fun line ->
        let coords = Str.split_delim (Str.regexp " -> ") line in
        coords)
      |> List.map ~f:(fun coords -> List.map coords ~f:vec2_of_string)
    in
    let max_r =
      List.map lines ~f:(fun line ->
        List.map line ~f:(fun (r, _c) -> r)
        |> List.max_elt ~compare:Int.compare
        |> Option.value_exn)
      |> List.max_elt ~compare:Int.compare
      |> Option.value_exn
    in
    let max_c = source_col + max_r + 12 in
    let min_c = source_col - max_r - 12 in
    let grid =
      let grid =
        Array.init
          (max_r + 1 + cave_floor_dist)
          ~f:(fun _r -> Array.create ~len:(max_c + 1) Cell.Air)
      in
      Grid.set_exn grid source_pos Source;
      let slide a b ~f =
        let delta = delta b a in
        let p = ref a in
        while Vec2.(!p <> b) do
          f !p;
          p := Vec2.(!p + delta)
        done;
        f b
      in
      let `Fill_floor =
        let a = max_r + cave_floor_dist, 0 in
        let b = max_r + cave_floor_dist, max_c in
        slide a b ~f:(fun p -> Grid.set_exn grid p Rock);
        `Fill_floor
      in
      List.iter lines ~f:(fun line ->
        let a = List.drop_last_exn line in
        let b = List.drop line 1 in
        List.iter (List.zip_exn a b) ~f:(fun (a, b) ->
          slide a b ~f:(fun p -> Grid.set_exn grid p Rock)));
      grid
    in
    { max_r; min_c; max_c; grid }
  ;;
end

let get_landing =
  let down = 1, 0 in
  let down_left = 1, -1 in
  let down_right = 1, 1 in
  fun (input : Input.t) ->
    Sequence.unfold_step ~init:Input.source_pos ~f:(fun pos ->
      let candidates = Vec2.[ pos + down; pos + down_left; pos + down_right ] in
      match
        List.find candidates ~f:(fun candidate ->
          match Grid.get_exn input.grid candidate with
          | Air -> true
          | Rock -> false
          | Source -> failwith "hit source"
          | Sand -> false)
      with
      | None -> Yield (pos, (-1, -1))
      | Some candidate -> Skip candidate)
    |> Sequence.hd_exn
;;

module A = struct
  let solve (input : Input.t) =
    let count =
      Sequence.unfold_step ~init:0 ~f:(fun count ->
        let landing = get_landing input in
        match
          let r, _c = landing in
          r < input.max_r
        with
        | false -> Yield (count, -1)
        | true ->
          Grid.set_exn input.grid landing Sand;
          Skip (count + 1))
      |> Sequence.hd_exn
    in
    count
  ;;

  let%expect_test _ =
    let input =
      Input.of_string {|498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9|}
    in
    let answer = solve input in
    print_endline [%string "%{input#Input}"];
    print_s [%sexp (answer : int)];
    [%expect
      {|
                         +

                         o
                        ooo
                       #ooo##
                      o#ooo#
                     ###ooo#
                       oooo#
                    o ooooo#
                   #########

    ###########################################
    24 |}]
  ;;

  let%expect_test _ =
    let input = In_channel.read_all "../input/input14.txt" |> Input.of_string in
    print_s [%sexp (solve input : int)];
    [%expect {| 892 |}]
  ;;
end

module B = struct
  let solve (input : Input.t) =
    let count =
      Sequence.unfold_step ~init:0 ~f:(fun count ->
        let landing = get_landing input in
        let count = count + 1 in
        match Vec2.(landing = Input.source_pos) with
        | true -> Yield (count, -1)
        | false ->
          Grid.set_exn input.grid landing Sand;
          Skip count)
      |> Sequence.hd_exn
    in
    count
  ;;

  let%expect_test _ =
    let input =
      Input.of_string {|498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9|}
    in
    let answer = solve input in
    print_endline [%string "%{input#Input}"];
    print_s [%sexp (answer : int)];
    [%expect
      {|
                         +
                        ooo
                       ooooo
                      ooooooo
                     oo#ooo##o
                    ooo#ooo#ooo
                   oo###ooo#oooo
                  oooo oooo#ooooo
                 oooooooooo#oooooo
                ooo#########ooooooo
               ooooo       ooooooooo
    ###########################################
    93 |}]
  ;;

  let%expect_test _ =
    let input = In_channel.read_all "../input/input14.txt" |> Input.of_string in
    print_s [%sexp (solve input : int)];
    [%expect {|
    27155 |}]
  ;;
end
