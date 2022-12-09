open Core

module Food = struct
  type t = { calories : int } [@@deriving sexp, fields]

  let of_calories calories = { calories }
end

module Elf = struct
  type t = { foods : Food.t list } [@@deriving sexp]

  let of_foods foods = { foods }
  let total_calores t = List.sum (module Int) t.foods ~f:Food.calories
end

let read_elves () =
  let lines = In_channel.(input_lines stdin) in
  List.folding_map lines ~init:[] ~f:(fun foods line ->
    match line with
    | "" ->
      let elf = Elf.of_foods foods in
      [], Some elf
    | _ ->
      let food = Food.of_calories (Int.of_string line) in
      food :: foods, None)
  |> List.filter_opt
;;

let run which =
  let elves = read_elves () in
  let total_calories =
    List.map elves ~f:Elf.total_calores |> List.sort ~compare:Int.descending
  in
  match which with
  | `A ->
    let answer = List.max_elt total_calories ~compare:Int.compare |> Option.value_exn in
    print_s [%sexp (answer : int)]
  | `B ->
    let top_three =
      let total_calories = List.sort total_calories ~compare:Int.descending in
      List.take total_calories 3
    in
    let answer = List.sum (module Int) top_three ~f:Fn.id in
    print_s [%sexp (answer : int)]
;;
