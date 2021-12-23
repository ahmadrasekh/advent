open Utils.Util
open Sexplib.Std
open Utils.LinearAlgebra

type input = string list [@@deriving sexp_of]
type pattern = string list [@@deriving sexp_of]
type t = (pattern * input) list [@@deriving sexp_of]

let parse_line str = str |>  
                     Str.split (Str.regexp " | ") |>
                     List.map (Str.split (Str.regexp " "))
                     |> fun (a::b::[]) -> (a,b);;

let parse_input () = "./input" |>
                     readFile |>
                     List.map parse_line


let isUniqu str = match (String.length str) with
  | 2 | 3 | 4 | 7 -> true
  | _ -> false


module SS = Set.Make(String)

let diff x y  = 
  let s = x |> Str.split (Str.regexp "") |> SS.of_list in
  let t = y |> Str.split (Str.regexp "") |> SS.of_list in
  let d = SS.diff s t in 
  SS.cardinal d

let mkMapping input = 
  let one = List.find (fun x -> (String.length x) = 2) input in
  let four = List.find (fun x -> (String.length x) = 4 ) input in
  let decode' str = match (String.length str), (diff str one), (diff str four) with
    | (2, _, _) -> 1 (** 1 *)
    | (4, _, _) -> 4 (** 4 *)
    | (3, _, _) -> 7 (** 7 *)
    | (7, _, _) -> 8 (** 8 *)
    | (6, 4, 3) -> 0 (** 0 *)
    | (6, 5, 3) -> 6 (** 6 *)
    | (6, 4, 2) -> 9 (** 9 *)
    | (5, 4, 3) -> 2 (** 2 *)
    | (5, 3, 2) -> 3 (** 3 *)
    | (5, 4, 2) -> 5 (** 5 *) in
  List.map (fun str -> (sortstr str, decode' str )) input


let decode mapping input = 
  List.map (fun x -> List.assoc (sortstr x) mapping) input |>
  List.map string_of_int |>
  String.concat "" |>
  int_of_string

let aux (pattern, input) =
  let mapping = mkMapping pattern in 
  decode mapping input

let () = parse_input () |>
         List.map aux |>
         List.fold_left (+) 0 |>
         Core.Int.sexp_of_t |>
         Core.Sexp.to_string |> 
         print_endline

