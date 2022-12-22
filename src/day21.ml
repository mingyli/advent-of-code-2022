open Core

module Equation = struct
  type operation =
    | Plus
    | Minus
    | Multiply
    | Divide
  [@@deriving sexp]

  let operation_of_string = function
    | "+" -> Plus
    | "-" -> Minus
    | "*" -> Multiply
    | "/" -> Divide
    | _ -> assert false
  ;;

  type expression =
    | Int of int
    | Binary of string * operation * string
  [@@deriving sexp]

  let expression_of_string s =
    match String.split s ~on:' ' with
    | [ int ] -> Int (Int.of_string int)
    | [ a; op; b ] -> Binary (a, operation_of_string op, b)
    | _ -> assert false
  ;;

  type t =
    { lhs : string
    ; rhs : expression
    }
  [@@deriving sexp]

  let of_string s =
    match Str.split (Str.regexp ": ") s with
    | [ lhs; rhs ] -> { lhs; rhs = expression_of_string rhs }
    | _ -> assert false
  ;;
end

module Input = struct
  type t = Equation.t list [@@deriving sexp]

  let of_string s = String.split_lines s |> List.map ~f:Equation.of_string
end

let solve (input : Input.t) =
  let equations =
    List.map input ~f:(fun equation -> equation.lhs, equation.rhs)
    |> String.Map.of_alist_exn
  in
  let rec eval name =
    match Map.find_exn equations name with
    | Int i -> i
    | Binary (a, op, b) ->
      let a = eval a in
      let b = eval b in
      (match op with
       | Plus -> a + b
       | Minus -> a - b
       | Multiply -> a * b
       | Divide -> a / b)
  in
  eval "root"
;;

let%expect_test _ =
  let input =
    Input.of_string
      {|root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32|}
  in
  print_s [%sexp (solve input : int)];
  [%expect {| 152 |}]
;;

let%expect_test _ =
  let input = In_channel.read_all "../input/input21.txt" |> Input.of_string in
  print_s [%sexp (solve input : int)];
  [%expect {|
    268597611536314 |}]
;;

let solve (input : Input.t) =
  let equations =
    List.map input ~f:(fun equation ->
      match equation.lhs with
      | lhs -> lhs, equation.rhs)
    |> String.Table.of_alist_exn
  in
  let rec eval name =
    match name with
    | "humn" -> None
    | name ->
      (match Hashtbl.find_exn equations name with
       | Int i -> Some i
       | Binary (a, op, b) ->
         let a = eval a in
         let b = eval b in
         (match a, b with
          | None, _ | _, None -> None
          | Some a, Some b ->
            Some
              (match op with
               | Plus -> a + b
               | Minus -> a - b
               | Multiply -> a * b
               | Divide -> a / b)))
  in
  let values = String.Table.create () in
  let rec commit name value =
    Hashtbl.add_exn values ~key:name ~data:value;
    match Hashtbl.find_exn equations name with
    | Int i ->
      (match name with
       | "humn" -> value
       | _ -> raise_s [%message (name : string) (i : int)])
    | Binary (a, op, b) ->
      (match eval a, eval b with
       | None, None -> assert false
       | Some _, Some _ -> assert false
       | None, Some b ->
         let a_value =
           match op with
           | Plus -> value - b
           | Minus -> value + b
           | Multiply -> value / b
           | Divide -> value * b
         in
         commit a a_value
       | Some a, None ->
         let b_value =
           match op with
           | Plus -> value - a
           | Minus -> a - value
           | Multiply -> value / a
           | Divide -> a / value
         in
         commit b b_value)
  in
  let a, b =
    match Hashtbl.find_exn equations "root" with
    | Int _ -> assert false
    | Binary (a, _, b) -> a, b
  in
  match eval a, eval b with
  | Some value, None -> commit b value
  | None, Some value -> commit a value
  | _ -> assert false
;;

let%expect_test _ =
  let input =
    Input.of_string
      {|root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32|}
  in
  print_s [%sexp (solve input : int)];
  [%expect {| 301 |}]
;;

let%expect_test _ =
  let input = In_channel.read_all "../input/input21.txt" |> Input.of_string in
  print_s [%sexp (solve input : int)];
  [%expect {|
    3451534022348 |}]
;;
