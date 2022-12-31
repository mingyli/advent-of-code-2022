open Core

module Snafu = struct
  type digit =
    | Minus
    | Double_minus
    | Zero
    | One
    | Two
  [@@deriving sexp]

  let of_char = function
    | '0' -> Zero
    | '1' -> One
    | '2' -> Two
    | '-' -> Minus
    | '=' -> Double_minus
    | _ -> assert false
  ;;

  let to_char = function
    | Minus -> '-'
    | Double_minus -> '='
    | Zero -> '0'
    | One -> '1'
    | Two -> '2'
  ;;

  let digit_to_int = function
    | Minus -> -1
    | Double_minus -> -2
    | Zero -> 0
    | One -> 1
    | Two -> 2
  ;;

  type t = digit list [@@deriving sexp]

  let of_string s = String.to_list s |> List.rev |> List.map ~f:of_char
  let to_string t = List.rev t |> List.map ~f:to_char |> String.of_char_list

  let rec to_int t =
    match t with
    | [] -> 0
    | hd :: tl -> digit_to_int hd + (5 * to_int tl)
  ;;

  let zero = [ Zero ]

  let ( + ) a b =
    let add a b =
      match a, b with
      | Zero, x | x, Zero -> [ x ]
      | One, One -> [ Two ]
      | One, Two | Two, One -> [ Double_minus; One ]
      | One, Minus | Minus, One -> [ Zero ]
      | One, Double_minus | Double_minus, One -> [ Minus ]
      | Two, Double_minus | Double_minus, Two -> [ Zero ]
      | Two, Minus | Minus, Two -> [ One ]
      | Two, Two -> [ Minus; One ]
      | Minus, Minus -> [ Double_minus ]
      | Double_minus, Double_minus -> [ One; Minus ]
      | Double_minus, Minus | Minus, Double_minus -> [ Two; Minus ]
    in
    let rec aux a b =
      match a, b with
      | [], b -> b
      | a, [] -> a
      | Zero :: atl, bhd :: btl -> bhd :: aux atl btl
      | ahd :: atl, Zero :: btl -> ahd :: aux atl btl
      | ahd :: atl, bhd :: btl ->
        (match add ahd bhd with
         | [ hd ] -> hd :: aux atl btl
         | hd :: tl -> hd :: aux tl (aux atl btl)
         | _ -> assert false)
    in
    aux a b
  ;;

  let%expect_test _ =
    let t = of_string "2=-01" in
    print_s [%sexp (t : t)];
    print_s [%sexp (to_int t : int)];
    [%expect {|
      (One Zero Minus Double_minus Two)
      976 |}];
    let t = t + t in
    print_s [%sexp (t : t)];
    print_s [%sexp (to_int t : int)];
    [%expect {|
      (Two Zero Double_minus One Double_minus One)
      1952 |}]
  ;;
end

let%expect_test _ =
  let input =
    {|1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122|}
    |> String.split_lines
    |> List.map ~f:Snafu.of_string
  in
  let sum = List.sum (module Snafu) input ~f:Fn.id in
  print_s [%message (sum : Snafu.t) (Snafu.to_string sum : string)];
  [%expect
    {|
    ((sum (Zero Double_minus One Minus Double_minus Two))
     ("Snafu.to_string sum" 2=-1=0)) |}]
;;

let%expect_test _ =
  let input =
    In_channel.read_all "../input/input25.txt"
    |> String.split_lines
    |> List.map ~f:Snafu.of_string
  in
  let sum = List.sum (module Snafu) input ~f:Fn.id in
  print_s [%message (sum : Snafu.t) (Snafu.to_string sum : string)];
  [%expect
    {|
    ((sum
      (One Two Minus Minus Double_minus Zero Zero Two Zero Double_minus Minus
       Zero Minus Double_minus Zero Minus Two Double_minus Minus Two))
     ("Snafu.to_string sum" 2-=2-0=-0-=0200=--21)) |}]
;;
