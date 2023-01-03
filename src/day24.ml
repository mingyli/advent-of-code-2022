open Core

let right = 0, 1
let left = 0, -1
let down = 1, 0
let up = -1, 0

module Input = struct
  type blizzard =
    { pos : Vec2.t
    ; direction : Vec2.t
    }
  [@@deriving sexp]

  type t =
    { blizzards : blizzard list
    ; width : int
    ; height : int
    }
  [@@deriving sexp]

  let next_blizzards t =
    List.map t.blizzards ~f:(fun { pos; direction } ->
      let r, c = Vec2.(pos + direction) in
      let r =
        match r with
        | 0 -> t.height - 2
        | r when r = t.height - 1 -> 1
        | r -> r
      in
      let c =
        match c with
        | 0 -> t.width - 2
        | c when c = t.width - 1 -> 1
        | c -> c
      in
      let pos = r, c in
      { pos; direction })
  ;;

  let goal t = t.height - 1, t.width - 2

  let is_border t (r, c) =
    match r, c with
    | r, c when r = 0 && c <> 1 -> true
    | r, c when r = t.height - 1 && c <> t.width - 2 -> true
    | _, c when c = 0 -> true
    | _, c when c = t.width - 1 -> true
    | _ -> false
  ;;

  let of_string s =
    let rows = String.split_lines s in
    let height = List.length rows in
    let width = List.nth_exn rows 0 |> String.length in
    let blizzards =
      rows
      |> List.concat_mapi ~f:(fun r row ->
           String.to_list row
           |> List.filter_mapi ~f:(fun c ch ->
                let pos = r, c in
                let%bind.Option direction =
                  match ch with
                  | 'v' -> Some down
                  | '>' -> Some right
                  | '<' -> Some left
                  | '^' -> Some up
                  | _ -> None
                in
                Some { pos; direction }))
    in
    { blizzards; width; height }
  ;;
end

let solve (input : Input.t) =
  let q = Queue.singleton ((0, 1), 0, input) in
  let module Key = struct
    type t = Vec2.t * int [@@deriving hash, sexp, compare]
  end
  in
  let already_added = Hash_set.of_list (module Key) [ (0, 1), 0 ] in
  let answer = Set_once.create () in
  while (not (Queue.is_empty q)) && Set_once.is_none answer do
    let pos, steps, t = Queue.dequeue_exn q in
    (* print_s [%message (pos : Vec2.t) (steps : int) (t.blizzards : Input.blizzard list)]; *)
    (* print_s [%message (pos : Vec2.t) (steps : int)]; *)
    if Vec2.(pos = Input.goal t)
    then Set_once.set_exn answer [%here] steps
    else (
      let steps = steps + 1 in
      let blizzards = Input.next_blizzards t in
      let is_blizzard pos =
        List.map blizzards ~f:(fun blizzard -> blizzard.pos)
        |> List.exists ~f:(fun p -> Vec2.(p = pos))
      in
      let t = Input.{ t with blizzards } in
      let candidates =
        [ right; down; 0, 0; up; left ]
        |> List.map ~f:(fun dir -> Vec2.(dir + pos))
        |> List.filter ~f:(fun (r, c) -> r >= 0 && c >= 0)
        |> List.filter ~f:(Fn.non (Input.is_border t))
        |> List.filter ~f:(fun pos -> not (is_blizzard pos))
      in
      List.filter candidates ~f:(fun candidate ->
        not (Hash_set.mem already_added (candidate, steps)))
      |> List.iter ~f:(fun candidate ->
           Hash_set.add already_added (candidate, steps);
           Queue.enqueue q (candidate, steps, t)))
  done;
  Set_once.get_exn answer [%here]
;;

let run which =
  ignore which;
  let input = Input.of_string In_channel.(input_all stdin) in
  print_s [%sexp (solve input : int)]
;;

let%expect_test _ =
  let input =
    Input.of_string {|#.#####
#.....#
#>....#
#.....#
#...v.#
#.....#
#####.#|}
  in
  print_s [%sexp (solve input : int)];
  [%expect {| 10 |}]
;;

let%expect_test _ =
  let input = Input.of_string {|#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#|} in
  print_s [%sexp (solve input : int)];
  [%expect {| 18 |}]
;;

(* let%expect_test _ =
  let input = In_channel.read_all "../input/input24.txt" |> Input.of_string in
  print_s [%sexp (solve input : int)];
  [%expect {|
    ((pos (19 122)) (password 20494))
    20494 |}]
;; *)
