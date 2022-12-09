open Core

module Inode = struct
  type t =
    { files : int Filename.Table.t
    ; dirs : t Filename.Table.t
    ; parent : t option
    }

  let rec sexp_of_t t =
    [%message "" ~_:(t.files : int Filename.Table.t) ~_:(t.dirs : t Filename.Table.t)]
  ;;

  let create () =
    { files = Filename.Table.create (); dirs = Filename.Table.create (); parent = None }
  ;;

  let create_with_parent parent =
    { files = Filename.Table.create ()
    ; dirs = Filename.Table.create ()
    ; parent = Some parent
    }
  ;;

  let rec size t =
    let file_sizes = Hashtbl.data t.files in
    let dir_sizes = Hashtbl.data t.dirs |> List.map ~f:size in
    file_sizes @ dir_sizes |> List.sum (module Int) ~f:Fn.id
  ;;

  let rec to_list t =
    let dirs = Hashtbl.data t.dirs in
    t :: List.concat_map dirs ~f:to_list
  ;;
end

let make_filesystem lines =
  let root = Inode.create () in
  let curr = ref root in
  List.iter lines ~f:(fun line ->
    match String.split line ~on:' ' with
    | [ "$"; "cd"; dir ] ->
      curr
        := (match !curr with
            | { files = _; dirs; parent } ->
              (match dir with
               | "/" -> root
               | ".." -> Option.value_exn parent
               | dir -> Hashtbl.find_exn dirs dir))
    | [ "$"; "ls" ] -> ()
    | [ "dir"; dir ] ->
      (match !curr with
       | { files = _; dirs; parent = _ } ->
         let data = Inode.create_with_parent !curr in
         Hashtbl.add_exn dirs ~key:dir ~data)
    | [ size; file ] ->
      let size = Int.of_string size in
      (match !curr with
       | { files; dirs = _; parent = _ } -> Hashtbl.add_exn files ~key:file ~data:size)
    | _ -> failwith "bad");
  root
;;

let%expect_test _ =
  let lines =
    {|$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k|}
    |> String.split_lines
  in
  print_s [%sexp (make_filesystem lines : Inode.t)];
  [%expect
    {|
    (((b.txt 14848514) (c.dat 8504156))
     ((a (((f 29116) (g 2557) (h.lst 62596)) ((e (((i 584)) ())))))
      (d (((d.ext 5626152) (d.log 8033020) (j 4060174) (k 7214296)) ())))) |}]
;;

module A = struct
  let solve lines =
    let root = make_filesystem lines in
    let rec small_sizes (inode : Inode.t) =
      match inode with
      | { files = _; dirs; parent = _ } ->
        let child_size = Hashtbl.data dirs |> List.sum (module Int) ~f:small_sizes in
        let size = Inode.size inode in
        if size <= 100000 then size + child_size else child_size
    in
    small_sizes root
  ;;

  let%expect_test _ =
    let lines =
      {|$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k|}
      |> String.split_lines
    in
    print_s [%sexp (solve lines : int)];
    [%expect {| 95437 |}]
  ;;
end

module B = struct
  let total_space = 70_000_000
  let required_space = 30_000_000

  let solve lines =
    let root = make_filesystem lines in
    let total_used_space = Inode.size root in
    let total_unused_space = total_space - total_used_space in
    let candidates = Inode.to_list root in
    List.map candidates ~f:Inode.size
    |> List.filter ~f:(fun size -> total_unused_space + size >= required_space)
    |> List.min_elt ~compare:Int.compare
    |> Option.value_exn
  ;;

  let%expect_test _ =
    let lines =
      {|$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k|}
      |> String.split_lines
    in
    print_s [%sexp (solve lines : int)];
    [%expect {| 24933642 |}]
  ;;
end

let run which =
  let lines = In_channel.(input_lines stdin) in
  let solve =
    match which with
    | `A -> A.solve
    | `B -> B.solve
  in
  let answer = solve lines in
  print_s [%sexp (answer : int)]
;;
