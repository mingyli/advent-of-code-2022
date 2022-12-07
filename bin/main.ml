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
         | 1 -> Day01.run
         | 2 -> Day02.run
         | 3 -> Day03.run
         | 4 -> Day04.run
         | 5 -> Day05.run
         | 6 -> Day06.run
         | _ -> assert false
       in
       run which)
;;

let () = Command.run command
