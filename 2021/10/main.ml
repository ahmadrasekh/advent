open Utils.Util
open Sexplib.Std
open Utils.LinearAlgebra

(* type t = char option [@@deriving sexp_of] *)
type t = string list [@@deriving sexp_of]

let parse_line = Str.split (Str.regexp "")

let parse_input () = "./input" |>
                     readFile

let append stack char =
  match char, stack with
  | ')', '('::xs -> xs
  | '}', '{'::xs -> xs
  | ']', '['::xs -> xs
  | '>', '<'::xs -> xs
  | _, _ -> char :: stack

let unbalancedChars str =
  Core.String.fold str ~init:[] ~f:(
    fun accum char ->
      match char with
      | '{' | '}' | '(' | ')' | '[' | ']' | '<' | '>' -> append accum char
      | _ -> accum
  )

let isCorrupt str =
  str |>
  unbalancedChars |>
  (function  
    | [] -> false
    | ls -> 
      ls |> 
      List.rev |>
      List.find_opt (function  | ')' | ']' | '}' | '>' -> true | _ -> false) |>
      (function | Some _ -> true | _ -> false)
  )

let isIncomplete str =
  str |>
  unbalancedChars |>
  (function  
    | [] -> None
    | ls -> 
      ls |> 
      List.rev |>
      List.find_opt (function  | ')' | ']' | '}' | '>' -> true | _ -> false) |>
      (function | Some _ -> None | _ -> Some ls)
  )

let firsCorruptChar str = 
  str |>
  unbalancedChars |>
  List.rev |> List.find_opt (function  
      | ')' | ']' | '}' | '>' -> true
      | _ -> false
    )

let part_01 ls =
  ls |>
  List.filter_map firsCorruptChar |>
  List.fold_left (fun acc -> function
      |']' -> acc + 57
      |')' -> acc + 3 
      |'}' -> acc + 1197
      |'>' -> acc + 25137 ) 0 

let score ls = List.fold_left (fun acc x -> 
    match x with
    | '(' -> (acc*5)+1
    | '[' -> (acc*5)+2
    | '{' -> (acc*5)+3
    | '<' -> (acc*5)+4
  ) 0 ls

let part_02 ls = ls |>
                 List.filter_map isIncomplete |>
                 List.map score |>
                 List.sort compare |>
                 Array.of_list |>
                 (fun arr -> let i = (Array.length arr)/2 in arr.(i))

(* List.map (List.to_seq >> String.of_seq ) *)


(* let () = parse_input () |>
         part_01 |> 
         sexp_of_t |>
         Core.Sexp.to_string |>
         print_endline *)

let () = parse_input () |>
         part_02 |> 
         string_of_int |>
         (* sexp_of_t |>
            Core.Sexp.to_string |> *)
         print_endline
