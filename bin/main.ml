open Core
open Src

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
         | _ -> assert false
       in
       run which)
;;

let () = Command.run command
