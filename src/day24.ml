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
  let start _ = 0, 1

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

type stage =
  | First
  | Second
  | Third
[@@deriving hash, sexp, compare]

let solve (input : Input.t) =
  let module Key = struct
    module T = struct
      type t = Vec2.t * int * stage [@@deriving hash, sexp, compare]
    end

    include T
    include Hashable.Make (T)
  end
  in
  let q =
    let q = Key.Hash_queue.create () in
    Hash_queue.enqueue_back_exn q ((0, 1), 0, First) ((0, 1), 0, First);
    q
  in
  let blizzard_by_step =
    let seq =
      Sequence.unfold ~init:input ~f:(fun input ->
        let next_guy = Input.{ input with blizzards = Input.next_blizzards input } in
        Some (input, next_guy))
    in
    let l = Sequence.take seq 999 |> Sequence.to_list in
    List.mapi l ~f:(fun i t -> i, t) |> Int.Map.of_alist_exn
  in
  let answer = Set_once.create () in
  while (not (Hash_queue.is_empty q)) && Set_once.is_none answer do
    let pos, steps, stage = Hash_queue.dequeue_front_exn q in
    (* print_s [%message (pos : Vec2.t) (steps : int) (t.blizzards : Input.blizzard list)]; *)
    (* print_s [%message (pos : Vec2.t) (steps : int)]; *)
    match stage, pos with
    | Third, pos when Vec2.(pos = Input.goal input) ->
      Set_once.set_exn answer [%here] steps
    | First, pos when Vec2.(pos = Input.goal input) ->
      let (_ : [ `Key_already_present | `Ok ]) =
        Hash_queue.enqueue_back q (pos, steps, Second) (pos, steps, Second)
      in
      ()
    | Second, pos when Vec2.(pos = Input.start input) ->
      let (_ : [ `Key_already_present | `Ok ]) =
        Hash_queue.enqueue_back q (pos, steps, Third) (pos, steps, Third)
      in
      ()
    | _ ->
      let steps = steps + 1 in
      let t = Map.find_exn blizzard_by_step steps in
      let is_blizzard pos =
        List.map t.blizzards ~f:(fun blizzard -> blizzard.pos)
        |> List.exists ~f:(fun p -> Vec2.(p = pos))
      in
      let candidates =
        [ right; down; 0, 0; up; left ]
        |> List.map ~f:(fun dir -> Vec2.(dir + pos))
        |> List.filter ~f:(fun (r, c) -> r >= 0 && c >= 0 && r < t.height && c < t.width)
        |> List.filter ~f:(Fn.non (Input.is_border t))
        |> List.filter ~f:(fun pos -> not (is_blizzard pos))
      in
      List.filter candidates ~f:(fun _candidate -> true)
      |> List.iter ~f:(fun candidate ->
           let (_ : [ `Key_already_present | `Ok ]) =
             Hash_queue.enqueue_back q (candidate, steps, stage) (candidate, steps, stage)
           in
           ())
  done;
  Set_once.get_exn answer [%here]
;;

let run which =
  ignore which;
  let input = Input.of_string In_channel.(input_all stdin) in
  print_s [%sexp (solve input : int)]
;;

let%expect_test _ =
  let input = Input.of_string {|#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#|} in
  print_s [%sexp (solve input : int)];
  [%expect {| 54 |}]
;;

(* let%expect_test _ =
  let input = In_channel.read_all "../input/input24.txt" |> Input.of_string in
  print_s [%sexp (solve input : int)];
  [%expect {| 816 |}]
;; *)
