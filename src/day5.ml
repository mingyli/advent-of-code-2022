open Core

module Move = struct
  type t =
    { amount : int
    ; from : int
    ; to_ : int
    }

  (* This obeys the following pattern: 
     
  {v
  let pattern = 
    let open Re in 
    seq 
      [ str "move "
      ; group (rep digit)
      ; str " from "
      ; group (rep digit)
      ; str " to "
      ; group (rep digit)
      ]
    |> compile
  ;;
  v}*)
  let of_string s =
    match String.split s ~on:' ' with
    | [ "move"; amount; "from"; from; "to"; to_ ] ->
      let amount = Int.of_string amount in
      let from = Int.of_string from in
      let to_ = Int.of_string to_ in
      { amount; from; to_ }
    | _ -> failwith "bad guy"
  ;;
end

let boxes =
  [| "HTZD"
   ; "QRWTGCS"
   ; "PBFQNRCH"
   ; "LCNFHZ"
   ; "GLFQS"
   ; "VPWZBRCS"
   ; "ZFJ"
   ; "DLVZRHQ"
   ; "BHGNFZLD"
  |]
;;

module A = struct
  let solve boxes lines =
    let boxes =
      Array.map boxes ~f:(fun s -> String.to_list s |> List.rev |> Stack.of_list)
    in
    List.iter lines ~f:(fun line ->
      let move = Move.of_string line in
      let from = Array.get boxes (move.from - 1) in
      let to_ = Array.get boxes (move.to_ - 1) in
      for _ = 1 to move.amount do
        let ch = Stack.pop_exn from in
        Stack.push to_ ch
      done);
    Array.to_list boxes |> List.map ~f:Stack.top_exn |> String.of_char_list
  ;;

  let%expect_test _ =
    let small = [| "ZN"; "MCD"; "P" |] in
    let lines =
      String.split_lines
        {|move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2|}
    in
    print_s [%sexp (solve small lines : string)];
    [%expect {| CMZ |}]
  ;;
end

module B = struct
  let solve boxes lines =
    let boxes =
      Array.map boxes ~f:(fun s -> String.to_list s |> List.rev |> Stack.of_list)
    in
    List.iter lines ~f:(fun line ->
      let move = Move.of_string line in
      let from = Array.get boxes (move.from - 1) in
      let to_ = Array.get boxes (move.to_ - 1) in
      let taken =
        let taken = Stack.create () in
        for _ = 1 to move.amount do
          let ch = Stack.pop_exn from in
          Stack.push taken ch
        done;
        taken
      in
      for _ = 1 to move.amount do
        let ch = Stack.pop_exn taken in
        Stack.push to_ ch
      done);
    Array.to_list boxes |> List.map ~f:Stack.top_exn |> String.of_char_list
  ;;

  let%expect_test _ =
    let small = [| "ZN"; "MCD"; "P" |] in
    let lines =
      String.split_lines
        {|move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2|}
    in
    print_s [%sexp (solve small lines : string)];
    [%expect {| MCD |}]
  ;;
end

let run which =
  let lines = In_channel.(input_lines stdin) in
  let solve =
    match which with
    | `A -> A.solve
    | `B -> B.solve
  in
  let answer = solve boxes lines in
  print_s [%sexp (answer : string)]
;;
