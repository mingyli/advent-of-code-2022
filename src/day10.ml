open Core

module Input = struct
  type op =
    | Noop
    | Addx of int

  type t = op list

  let of_string s =
    String.split_lines s
    |> List.map ~f:(fun line ->
         match String.split line ~on:' ' with
         | [ "noop" ] -> Noop
         | [ "addx"; v ] -> Addx (Int.of_string v)
         | _ -> failwith "bad line")
  ;;

  let example =
    of_string
      {|addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop|}
  ;;
end

module A = struct
  type state =
    { x : int
    ; cycle : int
    ; sum : int
    }

  let solve (input : Input.t) =
    let strength cycle x =
      match cycle with
      | 20 | 60 | 100 | 140 | 180 | 220 -> cycle * x
      | _ -> 0
    in
    let final_state =
      List.fold input ~init:{ x = 1; cycle = 1; sum = 0 } ~f:(fun { x; cycle; sum } op ->
        let cycle = cycle + 1 in
        match op with
        | Noop -> { cycle; x; sum = sum + strength cycle x }
        | Addx v ->
          let sum = sum + strength cycle x in
          let x = x + v in
          let cycle = cycle + 1 in
          let sum = sum + strength cycle x in
          { cycle; x; sum })
    in
    final_state.sum
  ;;

  let%expect_test _ =
    let input = Input.example in
    print_s [%sexp (solve input : int)];
    [%expect {| 13140 |}]
  ;;

  let%expect_test _ =
    let input = In_channel.read_all "../input/input10.txt" |> Input.of_string in
    print_s [%sexp (solve input : int)];
    [%expect {| 13220 |}]
  ;;
end

module B = struct
  let solve (input : Input.t) =
    let x = ref 1 in
    let cycle = ref 1 in
    let screen = Queue.create () in
    let draw () =
      match ((!cycle - 1) mod 40) - !x with
      | -1 | 0 | 1 -> Queue.enqueue screen '#'
      | _ -> Queue.enqueue screen ' '
    in
    List.iter input ~f:(fun op ->
      match op with
      | Noop ->
        draw ();
        incr cycle
      | Addx v ->
        draw ();
        incr cycle;
        draw ();
        incr cycle;
        x := !x + v);
    let screen = Queue.to_list screen |> List.chunks_of ~length:40 in
    print_endline "";
    List.map screen ~f:String.of_char_list |> String.concat ~sep:"\n"
  ;;

  let%expect_test _ =
    let input = Input.example in
    let output = solve input in
    String.split_lines output |> List.iter ~f:print_endline;
    [%expect
      {|
      ##  ##  ##  ##  ##  ##  ##  ##  ##  ##
      ###   ###   ###   ###   ###   ###   ###
      ####    ####    ####    ####    ####
      #####     #####     #####     #####
      ######      ######      ######      ####
      #######       #######       ####### |}]
  ;;

  let%expect_test _ =
    let input = In_channel.read_all "../input/input10.txt" |> Input.of_string in
    String.split_lines (solve input) |> List.iter ~f:print_endline;
    [%expect {|
      ###  #  #  ##  #  # #  # ###  #### #  #
      #  # #  # #  # # #  #  # #  # #    # #
      #  # #  # #  # ##   #### ###  ###  ##
      ###  #  # #### # #  #  # #  # #    # #
      # #  #  # #  # # #  #  # #  # #    # #
      #  #  ##  #  # #  # #  # ###  #### #  # |}]
  ;;
end

let run which =
  let input = Input.of_string In_channel.(input_all stdin) in
  match which with
  | `A ->
    let output = A.solve input in
    print_s [%sexp (output : int)]
  | `B ->
    let output = B.solve input in
    print_endline output
;;
