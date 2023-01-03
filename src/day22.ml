open Core

module Grid = struct
  include Grid.Make2 (Char)

  let print_row row = Array.to_list row |> String.of_char_list |> print_string

  let print t =
    print_endline "  0123456789";
    Array.iteri t ~f:(fun i row ->
      print_string [%string "%{i#Int} "];
      print_row row;
      print_endline "")
  ;;

  let in_bounds t pos =
    contains t pos
    &&
    match get_exn t pos with
    | '.' -> true
    | '#' | ' ' -> false
    | _ -> assert false
  ;;
end

module Path = struct
  type action =
    | Walk of int
    | Turn of [ `Clockwise | `Counterclockwise ]
  [@@deriving sexp]

  type t = action list [@@deriving sexp]

  let of_string t =
    String.strip t
    |> String.to_list
    |> List.group ~break:(fun left right ->
         Bool.( <> ) (Char.is_digit left) (Char.is_digit right))
    |> List.map ~f:(fun s ->
         match String.of_char_list s with
         | "R" -> Turn `Clockwise
         | "L" -> Turn `Counterclockwise
         | num -> Walk (Int.of_string num))
  ;;
end

let right = 0, 1
let left = 0, -1
let down = 1, 0
let up = -1, 0

let turn facing = function
  | `Clockwise ->
    if Vec2.(facing = right)
    then down
    else if Vec2.(facing = down)
    then left
    else if Vec2.(facing = left)
    then up
    else right
  | `Counterclockwise ->
    if Vec2.(facing = right)
    then up
    else if Vec2.(facing = up)
    then left
    else if Vec2.(facing = left)
    then down
    else right
;;

module Input = struct
  type t =
    { board : Grid.t
    ; path : Path.t
    }
  [@@deriving sexp]

  let of_string s =
    match Str.(split (regexp "\n\n") s) with
    | [ board; path ] ->
      let board =
        let lines = String.split_lines board in
        let width =
          List.map lines ~f:String.length
          |> List.max_elt ~compare:Int.compare
          |> Option.value_exn
        in
        Array.of_list lines
        |> Array.map ~f:(fun s ->
             Array.init width ~f:(fun i ->
               if i < String.length s then String.get s i else ' '))
      in
      let path = Path.of_string path in
      { board; path }
    | _ -> assert false
  ;;
end

let step ~which board pos facing =
  let height = Grid.height board in
  let width = Grid.width board in
  let next_pos, next_facing =
    match which with
    | `A ->
      let add pos facing =
        let r, c = Vec2.(pos + facing) in
        let r = (r + height) mod height in
        let c = (c + width) mod width in
        r, c
      in
      let hypothetical = add pos facing in
      (match Grid.get_exn board hypothetical with
       | '.' -> hypothetical, facing
       | '#' -> pos, facing
       | ' ' ->
         (* Wrap. *)
         let hypothetical =
           let curr = ref hypothetical in
           while Char.(Grid.get_exn board !curr = ' ') do
             curr := add !curr facing
           done;
           !curr
         in
         (match Grid.get_exn board hypothetical with
          | '.' -> hypothetical, facing
          | '#' -> pos, facing
          | _ -> assert false)
       | _ -> assert false)
    | `B -> assert false
  in
  next_pos, next_facing
;;

let solve ~which (input : Input.t) =
  (* print_s [%sexp (input : Input.t)]; *)
  (* Grid.print input.board;
  print_s [%sexp (input.path : Path.t)]; *)
  let path = Sequence.of_list input.path in
  let init =
    let first_row = input.board.(0) in
    let c = Array.findi_exn first_row ~f:(fun _c ch -> Char.(ch <> ' ')) |> fst in
    0, c
  in
  let bruh =
    Sequence.unfold_with path ~init:(init, right) ~f:(fun (pos, facing) action ->
      match action with
      | Turn clock ->
        let facing = turn facing clock in
        Yield ((pos, facing), (pos, facing))
      | Walk walk ->
        let pos, facing =
          Sequence.unfold_step ~init:(pos, facing, 0) ~f:(fun (pos, facing, steps) ->
            if steps = walk
            then Yield ((pos, facing), (pos, facing, steps))
            else (
              let hypothetical, facing = step ~which input.board pos facing in
              Skip (hypothetical, facing, steps + 1)))
          |> Sequence.hd_exn
        in
        Yield ((pos, facing), (pos, facing)))
  in
  let bruh = Sequence.to_list bruh in
  let pos, facing = List.last_exn bruh in
  let password =
    let r, c = pos in
    let facing =
      if Vec2.(facing = right)
      then 0
      else if Vec2.(facing = up)
      then 3
      else if Vec2.(facing = left)
      then 2
      else 1
    in
    (1000 * (r + 1)) + (4 * (c + 1)) + facing
  in
  print_s [%message (pos : Vec2.t) (password : int)];
  password
;;

let run which =
  let input = Input.of_string In_channel.(input_all stdin) in
  print_s [%sexp (solve ~which input : int)]
;;

let%expect_test _ =
  let input = In_channel.read_all "../input/input22-small.txt" |> Input.of_string in
  print_s [%sexp (solve ~which:`A input : int)];
  [%expect {|
    ((pos (5 7)) (password 6032))
    6032 |}]
;;

let%expect_test _ =
  let input = In_channel.read_all "../input/input22.txt" |> Input.of_string in
  print_s [%sexp (solve ~which:`A input : int)];
  [%expect {|
    ((pos (19 122)) (password 20494))
    20494 |}]
;;
