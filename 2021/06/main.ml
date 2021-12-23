open Utils.Util
open Sexplib.Std
open Utils.LinearAlgebra

type t = int list [@@deriving sexp_of];;
let parse_input ()= "./input" |>
                    readFile |>
                    List.map (Str.split (Str.regexp ",")) |>
                    List.flatten |>
                    List.map(int_of_string) 
(* 
(* 256/4 = 64 *)
(* 256/8 = 32 *)
let one = fun acc x -> match x with 
  | 0 -> 6::8::acc
  | _ -> (x-1)::acc

let six = (fun acc x -> match x with
    | 0 -> 1::3::acc
    | 1 -> 4::2::acc
    | 2 -> 3::5::acc
    | 3 -> 6::4::acc
    | 4 -> 5::7::acc
    | 5 -> 8::6::acc
    | 6 -> 0::acc
    | 7 -> 1::acc
    | 8 -> 2::acc)

let four = (fun acc x -> match x with
    | 0 -> 3::5::acc 
    | 1 -> 4::6::acc
    | 2 -> 5::7::acc
    | 3 -> 6::8::acc
    | 4 -> 0::acc
    | 5 -> 1::acc
    | 6 -> 2::acc
    | 7 -> 3::acc
    | 8 -> 4::acc)

(* let emulate' emulate days ls  = 
   let next = (emulateOne ls) in
   if days > 0 then emulate (days-1) next  else ls *)

let rec emulate days ls  = 
  let next =  List.fold_left one [] ls in
  if days > 0 then emulate (days-1) next  else ls



let  () = (parse_input ()) |>
          (emulate 40) |>
          (* List.flatten |> *)
          List.length |> 
          string_of_int |> 
          (* Core.Sexp.to_string |>
             sexp_of_t |> *)
          print_endline
 *)


let input = parse_input ();;

let initNest input = 
  let nest = Array.make 9 0 in
  List.iter (fun x -> let t = nest.(x) in nest.(x) <- t+1) input;
  nest

let lshift arr = 
  let tmp = Array.copy arr in
  let m = (Array.length arr)-1 in
  Array.iteri (
    fun i a -> 
      if i = 0 then
        tmp.(m) <- a
      else 
        tmp.(i-1) <- a
  ) arr; tmp

let emulateOne nest = 
  let nest' = lshift nest in 
  nest'.(6) <- nest'.(6) + nest'.(8);
  nest'

let rec emulate days nest = 
  if days > 0 then emulate (days-1) (emulateOne nest) else nest

let sum nest = Array.fold_left (+) 0 nest


let () = parse_input() |>
         initNest |> 
         emulate 256 |>
         sum |>
         string_of_int |>
         print_endline