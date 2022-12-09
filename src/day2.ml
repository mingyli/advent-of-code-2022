open Core

module Outcome = struct
  type t =
    | Win
    | Tie
    | Lose

  let score = function
    | Win -> 6
    | Tie -> 3
    | Lose -> 0
  ;;

  let of_string = function
    | "X" -> Lose
    | "Y" -> Tie
    | "Z" -> Win
    | _ -> assert false
  ;;
end

module Hand = struct
  type t =
    | Rock
    | Paper
    | Scissors

  let beats t t' : Outcome.t =
    match t, t' with
    | Rock, Scissors | Scissors, Paper | Paper, Rock -> Win
    | Rock, Rock | Paper, Paper | Scissors, Scissors -> Tie
    | Rock, Paper | Paper, Scissors | Scissors, Rock -> Lose
  ;;

  let of_opponent = function
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissors
    | _ -> assert false
  ;;

  let of_me = function
    | "Y" -> Paper
    | "X" -> Rock
    | "Z" -> Scissors
    | _ -> assert false
  ;;

  let score = function
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3
  ;;
end

module A = struct
  let score opponent me =
    let outcome = Hand.beats me opponent in
    let select = Hand.score me in
    select + Outcome.score outcome
  ;;

  let solve lines =
    List.map lines ~f:(fun line ->
      match String.split line ~on:' ' with
      | [ opponent; me ] ->
        let opponent = Hand.of_opponent opponent in
        let me = Hand.of_me me in
        let score = score opponent me in
        score
      | _ -> assert false)
    |> List.sum (module Int) ~f:Fn.id
  ;;

  let%expect_test _ =
    let lines = [ "A Y"; "B X"; "C Z" ] in
    print_s [%sexp (solve lines : int)];
    [%expect {| 15 |}]
  ;;
end

module B = struct
  let score (opponent : Hand.t) (outcome : Outcome.t) =
    let me =
      match outcome with
      | Tie -> opponent
      | Lose ->
        (match opponent with
         | Rock -> Scissors
         | Scissors -> Paper
         | Paper -> Rock)
      | Win ->
        (match opponent with
         | Rock -> Paper
         | Scissors -> Rock
         | Paper -> Scissors)
    in
    let select = Hand.score me in
    select + Outcome.score outcome
  ;;

  let solve lines =
    List.map lines ~f:(fun line ->
      match String.split line ~on:' ' with
      | [ opponent; outcome ] ->
        let opponent = Hand.of_opponent opponent in
        let outcome = Outcome.of_string outcome in
        opponent, outcome
      | _ -> assert false)
    |> List.map ~f:(fun (opponent, outcome) -> score opponent outcome)
    |> List.sum (module Int) ~f:Fn.id
  ;;

  let%expect_test _ =
    let lines = [ "A Y"; "B X"; "C Z" ] in
    print_s [%sexp (solve lines : int)];
    [%expect {| 12 |}]
  ;;
end

let run which =
  let lines = In_channel.(input_lines stdin) in
  let solve =
    match which with
    | `A -> A.solve
    | `B -> B.solve
  in
  let answer = solve lines in
  print_s [%sexp (answer : int)]
;;
