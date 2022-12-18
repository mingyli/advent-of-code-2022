open Core

module Input = struct
  module Cell = struct
    type t =
      | Start
      | End
      | Height of char
    [@@deriving sexp]

    let of_char = function
      | 'S' -> Start
      | 'E' -> End
      | ch -> Height ch
    ;;
  end

  include Grid.Make2 (Cell)

  let elevation (t : t) pos =
    Char.to_int
      (match get_exn t pos with
       | Height ch -> ch
       | Start -> 'a'
       | End -> 'z')
  ;;

  let of_string s =
    String.split_lines s
    |> List.map ~f:String.to_list
    |> List.map ~f:(List.map ~f:Cell.of_char)
    |> List.map ~f:Array.of_list
    |> Array.of_list
  ;;
end

module Pos = struct
  include Vec2

  let neighbors =
    let dirs = [ -1, 0; 1, 0; 0, -1; 0, 1 ] in
    fun (r, c) -> List.map dirs ~f:(fun d -> (r, c) + d)
  ;;
end

module A = struct
  let solve (input : Input.t) =
    let start =
      Array.find_mapi_exn input ~f:(fun r row ->
        Array.find_mapi row ~f:(fun c cell ->
          match cell with
          | Start -> Some (r, c)
          | End | Height _ -> None))
    in
    let q = Queue.singleton start in
    let dist = Pos.Table.of_alist_exn [ start, 0 ] in
    let answer = Set_once.create () in
    while (not (Queue.is_empty q)) && Set_once.is_none answer do
      let pos = Queue.dequeue_exn q in
      let elevation = Input.elevation input pos in
      Pos.neighbors pos
      |> List.filter ~f:(Input.contains input)
      |> List.filter ~f:(fun pos -> not (Hashtbl.mem dist pos))
      |> List.filter ~f:(fun pos -> Input.elevation input pos <= elevation + 1)
      |> List.iter ~f:(fun (r, c) ->
           let distance = 1 + Hashtbl.find_exn dist pos in
           Hashtbl.add_exn dist ~key:(r, c) ~data:distance;
           match Input.get_exn input (r, c) with
           | End -> Set_once.set_exn answer [%here] distance
           | Start | Height _ -> Queue.enqueue q (r, c))
    done;
    Set_once.get_exn answer [%here]
  ;;

  let%expect_test _ =
    let input = Input.of_string {|Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi|} in
    print_s [%sexp (solve input : int)];
    [%expect {| 31 |}]
  ;;

  (* let%expect_test _ =
    let input = In_channel.read_all "../input/input12.txt" |> Input.of_string in
    print_s [%sexp (solve input : int)];
    [%expect {| 447 |}]
  ;; *)
end

module B = struct
  let solve (input : Input.t) =
    let end' =
      Array.find_mapi_exn input ~f:(fun r row ->
        Array.find_mapi row ~f:(fun c cell ->
          match cell with
          | End -> Some (r, c)
          | Start | Height _ -> None))
    in
    let q = Queue.singleton end' in
    let dist = Pos.Table.of_alist_exn [ end', 0 ] in
    let answer = Set_once.create () in
    while (not (Queue.is_empty q)) && Set_once.is_none answer do
      let pos = Queue.dequeue_exn q in
      let elevation = Input.elevation input pos in
      Pos.neighbors pos
      |> List.filter ~f:(Input.contains input)
      |> List.filter ~f:(fun pos -> not (Hashtbl.mem dist pos))
      |> List.filter ~f:(fun pos -> elevation <= Input.elevation input pos + 1)
      |> List.iter ~f:(fun (r, c) ->
           let distance = 1 + Hashtbl.find_exn dist pos in
           Hashtbl.add_exn dist ~key:(r, c) ~data:distance;
           match Input.get_exn input (r, c) with
           | Start | Height 'a' ->
             Set_once.set_exn answer [%here] (Hashtbl.find_exn dist (r, c))
           | End | Height _ -> Queue.enqueue q (r, c))
    done;
    Set_once.get_exn answer [%here]
  ;;

  let%expect_test _ =
    let input = Input.of_string {|Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi|} in
    print_s [%sexp (solve input : int)];
    [%expect {| 29 |}]
  ;;

  (* let%expect_test _ =
    let input = In_channel.read_all "../input/input12.txt" |> Input.of_string in
    print_s [%sexp (solve input : int)];
    [%expect {| 446 |}]
  ;; *)
end

let run which =
  let input = Input.of_string In_channel.(input_all stdin) in
  match which with
  | `A ->
    let output = A.solve input in
    print_s [%sexp (output : int)]
  | `B ->
    let output = B.solve input in
    print_s [%sexp (output : int)]
;;
