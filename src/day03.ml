open Core

let priority ch =
  match ch with
  | ch when Char.(ch >= 'a' && ch <= 'z') -> Char.to_int ch - Char.to_int 'a' + 1
  | ch when Char.(ch >= 'A' && ch <= 'Z') -> Char.to_int ch - Char.to_int 'A' + 27
  | _ -> assert false
;;

module A = struct
  let solve lines =
    List.map lines ~f:(fun line ->
      let len = String.length line in
      let first = String.slice line 0 (len / 2) in
      let second = String.slice line (len / 2) len in
      let first = String.to_list first |> Char.Set.of_list in
      let second = String.to_list second |> Char.Set.of_list in
      Set.inter first second)
    |> List.sum (module Int) ~f:(Set.sum (module Int) ~f:priority)
  ;;

  let%expect_test _ =
    let inp =
      [ "vJrwpWtwJgWrhcsFMMfFFhFp"
      ; "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
      ; "PmmdzqPrVvPwwTWBwg"
      ; "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
      ; "ttgJtRGJQctTZtZT"
      ; "CrZsJsPPZsGzwwsLwLmpwMDw"
      ]
    in
    print_s [%sexp (solve inp : int)];
    [%expect {|
      157 |}]
  ;;
end

module B = struct
  let solve lines =
    let groups = List.chunks_of lines ~length:3 in
    List.map groups ~f:(fun group ->
      match group with
      | [ a; b; c ] ->
        let a = String.to_list a |> Char.Set.of_list in
        let b = String.to_list b |> Char.Set.of_list in
        let c = String.to_list c |> Char.Set.of_list in
        Set.inter a (Set.inter b c)
      | _ -> assert false)
    |> List.map ~f:(fun badge ->
         match Set.to_list badge with
         | [ badge ] -> badge
         | _ -> assert false)
    |> List.sum (module Int) ~f:priority
  ;;

  let%expect_test _ =
    let inp =
      [ "vJrwpWtwJgWrhcsFMMfFFhFp"
      ; "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
      ; "PmmdzqPrVvPwwTWBwg"
      ; "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
      ; "ttgJtRGJQctTZtZT"
      ; "CrZsJsPPZsGzwwsLwLmpwMDw"
      ]
    in
    print_s [%sexp (solve inp : int)];
    [%expect {|
      70 |}]
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
