open Core

module Direction = struct
  type t =
    | Left
    | Right
  [@@deriving sexp]

  let of_char = function
    | '<' -> Left
    | '>' -> Right
    | _ -> assert false
  ;;
end

module Input = struct
  type t = Direction.t list

  let of_string s = String.to_list s |> List.map ~f:Direction.of_char
end

module Rock = struct
  type t =
    { anchor : Vec2.t
    ; mask : Vec2.t list
    }
  [@@deriving sexp_of]

  let minus anchor = { anchor; mask = [ 2, 0; 3, 0; 4, 0; 5, 0 ] }
  let plus anchor = { anchor; mask = [ 3, 0; 2, 1; 3, 1; 4, 1; 3, 2 ] }
  let j anchor = { anchor; mask = [ 2, 0; 3, 0; 4, 0; 4, 1; 4, 2 ] }
  let i anchor = { anchor; mask = [ 2, 0; 2, 1; 2, 2; 2, 3 ] }
  let square anchor = { anchor; mask = [ 2, 0; 3, 0; 2, 1; 3, 1 ] }
  let cycle () = Sequence.cycle_list_exn [ minus; plus; j; i; square ]
  let guh t = List.map t.mask ~f:(fun p -> Vec2.(t.anchor + p))
  let down t = { t with anchor = Vec2.(t.anchor + (0, -1)) }

  let highest t =
    guh t |> List.map ~f:snd |> List.max_elt ~compare:Int.compare |> Option.value_exn
  ;;

  let in_bounds t = List.for_all (guh t) ~f:(fun (x, _y) -> x >= 0 && x < 7)
  let out_of_bounds t = not (in_bounds t)
end

module A = struct
  let print rocks y y' =
    for y = y downto y' do
      for x = 0 to 6 do
        match
          List.exists rocks ~f:(fun rock ->
            List.exists (Rock.guh rock) ~f:(fun p -> Vec2.(p = (x, y))))
        with
        | true -> print_string "#"
        | false -> print_string "."
      done;
      print_endline ""
    done
  ;;

  let contains grid (r : Vec2.t) =
    Queue.exists grid ~f:(fun rock ->
      List.exists (Rock.guh rock) ~f:(fun p -> Vec2.(p = r)))
  ;;

  let collides grid rock = List.exists (Rock.guh rock) ~f:(fun p -> contains grid p)

  let apply grid rock (jet : Direction.t) =
    let hypothetical =
      match jet with
      | Left -> Rock.{ rock with anchor = Vec2.(rock.anchor + (-1, 0)) }
      | Right -> Rock.{ rock with anchor = Vec2.(rock.anchor + (1, 0)) }
    in
    (* print_s [%message (jet : Direction.t) (hypothetical : Rock.t)]; *)
    match collides grid hypothetical || Rock.out_of_bounds hypothetical with
    | true -> rock
    | false -> hypothetical
  ;;

  let solve (input : Input.t) =
    let rocks = ref (Rock.cycle ()) in
    let jets = ref (Sequence.cycle_list_exn input) in
    let grid =
      Queue.singleton
        Rock.{ anchor = 0, 0; mask = [ 0, 0; 1, 0; 2, 0; 3, 0; 4, 0; 5, 0; 6, 0 ] }
    in
    let height = ref 0 in
    for _ = 1 to 2022 do
      let rock, rr = Sequence.next !rocks |> Option.value_exn in
      rocks := rr;
      let anchor = 0, !height + 3 + 1 in
      let rock =
        Sequence.unfold_step ~init:(rock anchor) ~f:(fun rock ->
          let jet, jj = Sequence.next !jets |> Option.value_exn in
          jets := jj;
          let rock = apply grid rock jet in
          let hypothetical = Rock.down rock in
          match collides grid hypothetical with
          | true -> Yield (rock, rock)
          | false -> Skip hypothetical)
        |> Sequence.hd_exn
      in
      Queue.enqueue grid rock;
      height := Int.max !height (Rock.highest rock);
      (* print (Queue.to_list grid) 9 0;
      print_endline ""; *)
      ()
    done;
    !height
  ;;

  let%expect_test _ =
    let input = Input.of_string {|>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>|} in
    print_s [%sexp (solve input : int)];
    [%expect {| 0 |}]
  ;;

  let%expect_test _ =
    let input =
      In_channel.read_all "../input/input17.txt" |> String.strip |> Input.of_string
    in
    print_s [%sexp (solve input : int)];
    [%expect {| 1828 |}]
  ;;
end

(* module B = struct 
  let solve (input:Input.t) = 

  let%expect_test _ = 
    let input = Input.of_string {||} in 
    print_s [%sexp (solve input : int)];

  
  let%expect_test _ =
    let input = In_channel.read_all "../input/input17.txt" |> Input.of_string in
    print_s [%sexp (solve input : int)];
    [%expect {| 1828 |}]
  ;;
end *)
