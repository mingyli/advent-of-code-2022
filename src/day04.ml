open Core

let parse lines =
  List.map lines ~f:(fun line ->
    match String.split line ~on:',' with
    | [ first; second ] ->
      let first =
        match String.split first ~on:'-' with
        | [ a; b ] -> Int.of_string a, Int.of_string b
        | _ -> assert false
      in
      let second =
        match String.split second ~on:'-' with
        | [ a; b ] -> Int.of_string a, Int.of_string b
        | _ -> assert false
      in
      first, second
    | _ -> assert false)
;;

module A = struct
  let contains first second =
    let a, b = first in
    let c, d = second in
    a >= c && b <= d
  ;;

  let solve lines =
    let intervals = parse lines in
    List.count intervals ~f:(fun (first, second) ->
      contains first second || contains second first)
  ;;

  let%expect_test _ =
    let lines = String.split_lines {|2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8|} in
    print_s [%sexp (solve lines : int)];
    [%expect {| 2 |}]
  ;;
end

module B = struct
  let overlap first second =
    let a, b = first in
    let c, d = second in
    if a <= c then b >= c else d >= a
  ;;

  let solve lines =
    let intervals = parse lines in
    List.count intervals ~f:(fun (first, second) -> overlap first second)
  ;;

  let%expect_test _ =
    let lines = String.split_lines {|2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8|} in
    print_s [%sexp (solve lines : int)];
    [%expect {| 4 |}]
  ;;
end

let run (which : Which.t) =
  let lines = In_channel.(input_lines stdin) in
  match which with
  | A ->
    let answer = A.solve lines in
    print_s [%sexp (answer : int)]
  | B ->
    let answer = B.solve lines in
    print_s [%sexp (answer : int)]
;;
