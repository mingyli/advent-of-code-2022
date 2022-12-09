open Core

let solve size line =
  let queue = Deque.create () in
  String.to_list line
  |> List.find_mapi_exn ~f:(fun i ch ->
       let i = i + 1 in
       let () =
         match Deque.length queue = size with
         | true ->
           Deque.drop_front queue;
           Deque.enqueue_back queue ch
         | false -> Deque.enqueue_back queue ch
       in
       match Deque.length queue = size with
       | false -> None
       | true ->
         (match
            Deque.to_list queue
            |> List.dedup_and_sort ~compare:Char.compare
            |> List.length
            = size
          with
          | true -> Some i
          | false -> None))
;;

module A = struct
  let solve line = solve 4 line

  let%expect_test _ =
    let solve line = print_s [%sexp (solve line : int)] in
    solve "mjqjpqmgbljsphdztnvjfqwrcgsmlb";
    solve "bvwbjplbgvbhsrlpgdmjqwftvncz";
    solve "nppdvjthqldpwncqszvftbrmjlhg";
    solve "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg";
    solve "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw";
    [%expect {|
      7
      5
      6
      10
      11 |}]
  ;;
end

module B = struct
  let solve line = solve 14 line

  let%expect_test _ =
    let solve line = print_s [%sexp (solve line : int)] in
    solve "mjqjpqmgbljsphdztnvjfqwrcgsmlb";
    solve "bvwbjplbgvbhsrlpgdmjqwftvncz";
    solve "nppdvjthqldpwncqszvftbrmjlhg";
    solve "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg";
    solve "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw";
    [%expect {|
      19
      23
      23
      29
      26 |}]
  ;;
end

let run which =
  let line = In_channel.(input_line_exn stdin) in
  let solve =
    match which with
    | `A -> A.solve
    | `B -> B.solve
  in
  let answer = solve line in
  print_s [%sexp (answer : int)]
;;
