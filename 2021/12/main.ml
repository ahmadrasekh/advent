open Utils.Util
open Sexplib.Std
open Utils.LinearAlgebra

type t = ((int * bool) array array) * int [@@deriving sexp_of]

let parse_line = 
  Str.split (Str.regexp "") >> 
  List.map (fun x -> int_of_string x, false)

let parse_input () = "./input" |>
                     read_file |>
                     List.map parse_line |>
                     List.map Array.of_list |>
                     Array.of_list

let rec deal_with_nines (m, flash_count) = 
  let coords = matrix_find_indices (fun (x, flashed) -> (x>9) && (not flashed)) m in
  match coords with 
  | [] -> (matrix_map (fun (x, _) -> (x, false)) m, flash_count)
  | _ -> 
    List.iter (fun (i,j) ->  
        let coords' = [
          (i-1,j-1);(i,j-1);(i+1,j-1);
          (i-1,j);(i+1,j);
          (i-1,j+1);(i,j+1);(i+1,j+1)] in 
        m.(j).(i) <- (0, true);
        List.iter (
          fun (x, y) -> 
            match  matrix_safe_get m (x, y) with
            | Some (n, b) when (not b) -> m.(y).(x) <- (succ n, b)
            | _ -> ()
        ) coords'
      ) coords; 
    deal_with_nines (m, flash_count + (List.length coords))

let step (m,flash_count) = 
  let m' = matrix_map (fun (n, b) -> (succ n, b)) m in
  (m', flash_count) |>
  deal_with_nines 

let rec part_01 counter input = 
  if counter=0 then 
    input 
  else 
    let input' = step input in
    part_01 (counter-1) input' 

let rec part_02 input = 
  let synced (m,flash_count) = 
    let test = Array.fold_left (
        fun acc row -> 
          let row_acc = Array.fold_left (fun c (x, _) -> x+c ) 0 row in
          acc+row_acc
      ) 0 m in (test = 0 ) in 
  let rec aux counter input = 
    if (synced input) 
    then counter 
    else 
      let input' = step input in
      aux (counter+1) input' 
  in aux 0 input 

let () = 
  parse_input () |>
  (fun x -> (x, 0)) |>
  part_02 |>
  (* sexp_of_t |> *)
  (* Core.Sexp.to_string |> *)
  string_of_int |>
  print_endline
