open Utils.Util
open Sexplib.Std


type reading = int list [@@deriving sexp_of]
type board = int list list [@@deriving sexp_of]
type t =  board  * int [@@deriving sexp_of]
(* type t = int [@@deriving sexp_of] *)
let parse_reading str = str |> 
                        Str.split (Str.regexp ",") |>
                        List.map(int_of_string) 

let parse_row (row:string) = row |>
                             Str.split (Str.regexp "[ ]+") |>
                             List.map(int_of_string)

let parse_board = List.map parse_row

let collect_rows ls = List.fold_left (fun (acc, curr) str ->  match str with
    | "" -> (match curr with 
        | [] -> (acc, [])
        | _ -> ((curr |> List.map(parse_row) |> List.rev)::acc, []))
    | _ -> (acc, str::curr)
  ) ([], []) ls |> fun (acc, curr) -> (curr |> List.map(parse_row) |> List.rev)::acc |> List.rev

let parse_input () = "./input" |>
                     read_file |>
                     fun (hd::tl) -> (parse_reading hd), (collect_rows tl)


let reduce n = List.map (fun m -> List.map (List.filter ((<>) n)) m)

let test reading (p: board list)  =
  Core.List.fold_until ~init:p
    ~f:(fun acc (n:int) ->
        let reduced = reduce n acc in
        let winner =  List.find_opt (fun x -> List.mem [] x) reduced in
        match winner with 
        | Some x -> Base.Continue_or_stop.Stop ((x, n))
        | None -> Base.Continue_or_stop.Continue (reduced)
      )
    ~finish:(fun winner -> winner |> List.flatten |> fun x -> (x, Core.List.last_exn reading )) reading

let part_01 (r, p) = p |>
                     List.map Utils.Util.transpose |>
                     (fun p' -> List.concat[p; p' ]) |>
                     test  r |> 
                     fun (winner, n) -> (List.flatten winner |> List.fold_left (+) 0 |> ( * ) n ) 

(* bug, returns one before actual reading *)
let test_ reading (p:board list)  = 
  Core.List.fold_until 
    ~init:p
    ~f:(
      fun p' n ->
        let reduced = reduce n p' in
        let filtered = List.filter (List.mem [] >> not ) reduced in
        match filtered with
        | last::[] -> Base.Continue_or_stop.Stop(last, n)
        | _ -> Base.Continue_or_stop.Continue(filtered)
    ) 
    ~finish:( List.flatten >> (fun x -> (x, List.hd reading)))
    reading
(* 
let part_02 (r, p) = p |>
                     test_ r |>
                     fun (winner, n) -> (List.flatten winner |> List.fold_left (+) 0, n) *)
let part_02 (r, p) = p |>
                     test_ r

(* |>
   fun (winner, n) -> (List.flatten winner |> List.fold_left (+) 0 |> ( * ) n )  *)

let reduce_board board reading = Core.List.fold_until
    ~init:board
    ~f:(
      fun b n ->
        let reduced = List.map (List.filter ((<>) n)) b in
        if List.mem [] reduced
        then Base.Continue_or_stop.Stop (reduced, n)
        else Base.Continue_or_stop.Continue (reduced)
    )
    ~finish:(fun last -> (last, ~-1))
    reading


let () = 
  let (reading, boards') = parse_input () in
  let boards = List.concat [boards'; Utils.Util.transpose boards' ] in
  let (board, n) = part_02 (reading, boards) in
  let remainder = Utils.Util.cut_list_at reading n in
  let last = reduce_board board remainder in
  (* parse_input () |>
     part_02|>  *)
  last |>
  sexp_of_t|>
  Core.Sexp.to_string |>
  print_endline