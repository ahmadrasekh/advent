open Utils.Util
open Sexplib.Std
open Utils.LinearAlgebra

type t = int list [@@deriving sexp_of];;

let parse_input ()= "./input" |>
                    readFile |>
                    List.map (Str.split (Str.regexp ",")) |>
                    List.flatten |>
                    List.map(int_of_string) 

let median ls =
  let ls' = Array.of_list ls in
  let ln = (List.length ls)/2 in
  ls'.(ln)

let mean ls = 
  (List.fold_left (+) 0 ls ) / ((List.length ls))

let fule ls m = 
  List.fold_left (fun acc x -> 
      acc + (abs (x - m))
    ) 0 ls

let sum n m = ((m - n + 1) * (m + n))/2

let fule' ls = 
  let m = mean ls in 
  List.fold_left (fun acc x -> 
      acc + (sum 1 (abs (x - m)))
    ) 0 ls

let () = parse_input() |>
         List.sort compare |>
         (* sexp_of_t |>
            Core.Sexp.to_string |> *)
         (* List.length |> *)
         (* (fun ls -> fule ls (mean ls)) |> *)
         fule' |> 
         string_of_int |> 
         (* string_of_float |>  *)
         print_endline
