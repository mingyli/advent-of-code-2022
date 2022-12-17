open Core
open Src

module Which = struct
  type t =
    [ `A
    | `B
    ]
  [@@deriving enumerate, sexp]

  let arg_type : t Command.Arg_type.t =
    Command.Arg_type.of_alist_exn [ "a", `A; "A", `A; "b", `B; "B", `B ]
  ;;
end

let command =
  Command.basic
    ~summary:"Advent of Code 2022"
    (let%map_open.Command () = return ()
     and day = anon ("day" %: int)
     and which = anon ("which" %: Which.arg_type) in
     fun () ->
       let run =
         match day with
         | 1 -> Day1.run
         | 2 -> Day2.run
         | 3 -> Day3.run
         | 4 -> Day4.run
         | 5 -> Day5.run
         | 6 -> Day6.run
         | 7 -> Day7.run
         | 8 -> Day8.run
         | 9 -> Day9.run
         | 10 -> Day10.run
         | 11 -> Day11.run
         | 12 -> Day12.run
         | 13 -> Day13.run
         | 15 -> Day15.run
         | 16 -> Day16.run
         | _ -> assert false
       in
       run which)
;;

let () = Command.run command
