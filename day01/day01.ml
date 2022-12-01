open Core

let cartesian l l' =
  List.concat (List.map ~f:(fun e -> List.map ~f:(fun e' -> (e, e')) l') l)

let read_numbers () =
  let file = In_channel.create "input" in
  List.map ~f:Int.of_string (In_channel.input_lines file)

let day01a numbers =
  let pairs = cartesian numbers numbers in
  List.find pairs ~f:(fun (a, b) -> a + b = 2020)

let day01b numbers =
  let set = Set.of_list (module Int) numbers in
  let pairs = cartesian numbers numbers in
  List.find_map pairs ~f:(fun (a, b) ->
      let complement = 2020 - a - b in
      if Set.mem set complement then Some (a, b, complement) else None)

let () =
  let numbers = read_numbers () in
  day01b numbers |> function
  | None -> assert false
  | Some (a, b, c) ->
      print_endline [%string "%{a#Int} %{b#Int} %{c#Int} %{a * b * c#Int}"]
