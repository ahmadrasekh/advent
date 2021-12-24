open Utils.Util
open Sexplib.Std
open Utils.LinearAlgebra

type elem = int option [@@deriving sexp_of]
type adjs = int list [@@deriving sexp_of]
type t = int array array [@@deriving sexp_of]

let parse_line str = str |> Str.split (Str.regexp "") |>  List.map int_of_string |> Array.of_list

let parse_input () = "./input" |>
                     readFile |>
                     Array.of_list |> 
                     Array.map parse_line

let safeGet m (x, y) = try Some m.(y).(x) with _ -> None

let getAdjs (i,j) m =
  List.filter_map (fun x -> safeGet m x) [(i, j); (i-1, j); (i+1, j); (i, j-1); (i, j+1)]

let isLowPoint (i,j) m  = 
  let adjs = getAdjs (i,j) m in
  List.fold_left (fun acc x -> ((List.hd adjs) < x) &&  acc ) true (List.tl adjs)

let getLowPointCoords m =
  Array.mapi ( fun j row ->
      row |> 
      Array.mapi (fun i x -> if (isLowPoint (i, j) m) then Some (i,j) else None) |>
      Array.to_list |>
      List.filter_map (fun x -> x) 
    ) m |>
  Array.to_list |>
  List.flatten

let part_01 m = 
  getLowPointCoords m |>
  List.fold_left (fun acc (x, y) -> m.(y).(x)+1+acc) 0

let boundaryFill (x, y) m = 
  let m' = Array.copy m in 
  let acc = ref [] in
  let rec aux (i, j) = match (safeGet m' (i, j)) with 
    | (Some x) when (x != 9) -> begin
        let _ = m'.(j).(i) <- 9 in
        let _ = (acc := (x:: (!acc))) in
        let a = aux  (i+1, j) in
        let b = aux  (i-1, j) in
        let c = aux  (i, j+1) in 
        let d = aux  (i, j-1) in
        !acc
      end
    | _ -> !acc in
  aux (x, y)

let part_02 m = 
  getLowPointCoords m |>
  (List.map (fun c -> boundaryFill c m)) |>
  List.sort (fun l1 l2 -> compare (List.length l1) (List.length l2)) |>
  List.rev |>
  (fun (x::y::z::_) -> [List.length x; List.length y * List.length z] )|>
  List.fold_left ( * ) 1 

let () = parse_input () |>
         part_02 |>
         string_of_int |>
         print_endline
