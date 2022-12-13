open Core

module Packet = struct
  type t =
    | Lis of t list
    | Num of int
  [@@deriving sexp]

  let rec of_chars chars : t * char list =
    match chars with
    | '[' :: rest ->
      let guy = Set_once.create () in
      let packets =
        Sequence.unfold_step ~init:rest ~f:(fun rest ->
          match rest with
          | ']' :: rest ->
            Set_once.set_exn guy [%here] rest;
            Done
          | _ ->
            let packet, rest = of_chars rest in
            let rest =
              match rest with
              | ',' :: rest -> rest
              | _ -> rest
            in
            Yield (packet, rest))
        |> Sequence.to_list
      in
      Lis packets, Set_once.get_exn guy [%here]
    | chars ->
      let digits, rest = List.split_while chars ~f:Char.is_digit in
      let num = String.of_char_list digits |> Int.of_string in
      Num num, rest
  ;;

  let of_string s =
    match of_chars (String.to_list s) with
    | t, [] -> t
    | t, l -> raise_s [%message "bad of_string" (t : t) (l : char list)]
  ;;

  let rec compare t1 t2 =
    match t1, t2 with
    | Num n1, Num n2 -> Int.compare n1 n2
    | Lis ts1, Num n2 -> compare (Lis ts1) (Lis [ Num n2 ])
    | Num n1, Lis ts2 -> compare (Lis [ Num n1 ]) (Lis ts2)
    | Lis ts1, Lis ts2 -> List.compare compare ts1 ts2
  ;;
end

module A = struct
  module Input = struct
    type t = (Packet.t * Packet.t) list [@@deriving sexp]

    let of_string s =
      String.split_lines s
      |> List.chunks_of ~length:3
      |> List.map ~f:(fun chunk ->
           match chunk with
           | [ left; right ] | [ left; right; "" ] ->
             Packet.of_string left, Packet.of_string right
           | _ -> failwith "bad chunks")
    ;;
  end

  let solve (input : Input.t) =
    List.filter_mapi input ~f:(fun i (t1, t2) ->
      let i = i + 1 in
      match Ordering.of_int (Packet.compare t1 t2) with
      | Less -> Some i
      | Equal -> failwith "got equal"
      | Greater -> None)
    |> List.sum (module Int) ~f:Fn.id
  ;;

  let%expect_test _ =
    let input =
      Input.of_string
        {|[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]|}
    in
    print_s [%sexp (solve input : int)];
    [%expect {|
      13 |}]
  ;;

  let%expect_test _ =
    let input = In_channel.read_all "../input/input13.txt" |> Input.of_string in
    print_s [%sexp (solve input : int)];
    [%expect {| 5938 |}]
  ;;
end

module B = struct
  module Input = struct
    type t = Packet.t list [@@deriving sexp]

    let of_string s =
      String.split_lines s
      |> List.filter ~f:(fun line -> not (String.is_empty line))
      |> List.map ~f:Packet.of_string
    ;;
  end

  let solve (input : Input.t) =
    let two : Packet.t = Lis [ Lis [ Num 2 ] ] in
    let six : Packet.t = Lis [ Lis [ Num 6 ] ] in
    let input = two :: six :: input in
    let input = List.sort input ~compare:Packet.compare in
    let two =
      List.find_mapi_exn input ~f:(fun i packet ->
        if [%compare.equal: Packet.t] packet two then Some (i + 1) else None)
    in
    let six =
      List.find_mapi_exn input ~f:(fun i packet ->
        if [%compare.equal: Packet.t] packet six then Some (i + 1) else None)
    in
    two * six
  ;;

  let%expect_test _ =
    let input =
      Input.of_string
        {|[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]|}
    in
    print_s [%sexp (solve input : int)];
    [%expect {|
      140 |}]
  ;;

  let%expect_test _ =
    let input = In_channel.read_all "../input/input13.txt" |> Input.of_string in
    print_s [%sexp (solve input : int)];
    [%expect {| 29025 |}]
  ;;
end

let run which =
  match which with
  | `A ->
    let input = A.Input.of_string In_channel.(input_all stdin) in
    let output = A.solve input in
    print_s [%sexp (output : int)]
  | `B ->
    let input = B.Input.of_string In_channel.(input_all stdin) in
    let output = B.solve input in
    print_s [%sexp (output : int)]
;;
