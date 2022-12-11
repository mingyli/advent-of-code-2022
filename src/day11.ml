open Core

module Monkey = struct
  type t =
    { id : int
    ; items : int Deque.t
    ; operation : int -> int
    ; test : int
    ; if_true : int
    ; if_false : int
    }
  [@@deriving sexp, fields]
end

module Input = struct
  type t = Monkey.t Int.Map.t [@@deriving sexp]

  let example () : t =
    let monkey id items operation test if_true if_false : Monkey.t =
      { id; items = Deque.of_array items; operation; test; if_true; if_false }
    in
    Int.Map.of_alist_exn
      [ 0, monkey 0 [| 79; 98 |] (fun old -> old * 19) 23 2 3
      ; 1, monkey 1 [| 54; 65; 75; 74 |] (fun old -> old + 6) 19 2 0
      ; 2, monkey 2 [| 79; 60; 97 |] (fun old -> old * old) 13 1 3
      ; 3, monkey 3 [| 74 |] (fun old -> old + 3) 17 0 1
      ]
  ;;

  let input () : t =
    let monkey id items operation test if_true if_false : Monkey.t =
      { id; items = Deque.of_array items; operation; test; if_true; if_false }
    in
    Int.Map.of_alist_exn
      [ 0, monkey 0 [| 98; 89; 52 |] (fun old -> old * 2) 5 6 1
      ; 1, monkey 1 [| 57; 95; 80; 92; 57; 78 |] (fun old -> old * 13) 2 2 6
      ; 2, monkey 2 [| 82; 74; 97; 75; 51; 92; 83 |] (fun old -> old + 5) 19 7 5
      ; 3, monkey 3 [| 97; 88; 51; 68; 76 |] (fun old -> old + 6) 7 0 4
      ; 4, monkey 4 [| 63 |] (fun old -> old + 1) 17 0 1
      ; 5, monkey 5 [| 94; 91; 51; 63 |] (fun old -> old + 4) 13 4 3
      ; 6, monkey 6 [| 61; 54; 94; 71; 74; 68; 98; 83 |] (fun old -> old + 2) 3 2 7
      ; 7, monkey 7 [| 90; 56 |] (fun old -> old * old) 11 3 5
      ]
  ;;
end

module A = struct
  let solve (input : Input.t) =
    let inspections = Int.Table.create () in
    for _round = 1 to 20 do
      Map.iter input ~f:(fun monkey ->
        while not (Deque.is_empty monkey.items) do
          Int.Table.incr inspections monkey.id;
          let item = Deque.dequeue_front_exn monkey.items in
          let worry = monkey.operation item in
          let worry = worry / 3 in
          let toward =
            match worry mod monkey.test = 0 with
            | true -> monkey.if_true
            | false -> monkey.if_false
          in
          let toward = Map.find_exn input toward in
          Deque.enqueue_back toward.items worry
        done)
    done;
    let inspections = Hashtbl.data inspections |> List.sort ~compare:Int.descending in
    match inspections with
    | a :: b :: _ ->
      let monkey_business = a * b in
      monkey_business
    | _ -> failwith "bad inspections"
  ;;

  let%expect_test _ =
    let input = Input.example () in
    print_s [%sexp (solve input : int)];
    [%expect {| 10605 |}]
  ;;
end

module B = struct
  let solve (input : Input.t) =
    let inspections = Int.Table.create () in
    let modulo =
      Map.data input |> List.map ~f:Monkey.test |> List.reduce_exn ~f:Int.( * )
    in
    for _round = 1 to 10_000 do
      Map.iter input ~f:(fun monkey ->
        while not (Deque.is_empty monkey.items) do
          Int.Table.incr inspections monkey.id;
          let item = Deque.dequeue_front_exn monkey.items in
          let worry = monkey.operation item in
          let worry = worry mod modulo in
          let toward =
            match worry mod monkey.test = 0 with
            | true -> monkey.if_true
            | false -> monkey.if_false
          in
          let toward = Map.find_exn input toward in
          Deque.enqueue_back toward.items worry
        done)
    done;
    let inspections = Hashtbl.data inspections |> List.sort ~compare:Int.descending in
    match inspections with
    | a :: b :: _ ->
      let monkey_business = a * b in
      monkey_business
    | _ -> failwith "bad inspections"
  ;;

  let%expect_test _ =
    let input = Input.example () in
    print_s [%sexp (solve input : int)];
    [%expect {| 2713310158 |}]
  ;;
end

let run which =
  let input = Input.input () in
  match which with
  | `A ->
    let output = A.solve input in
    print_s [%sexp (output : int)]
  | `B ->
    let output = B.solve input in
    print_s [%sexp (output : int)]
;;
