open Utils.Util
open Sexplib.Std

type reading = int list [@@deriving sexp_of]
type row = (int * bool) list [@@deriving sexp_of]
type matrix = row list [@@deriving sexp_of]
type problem = (reading * matrix list) [@@deriving sexp_of]

let parse_reading str = str |> 
                        Str.split (Str.regexp ",") |>
                        List.map(int_of_string) 

let parse_row (row:string) = row |>
                             Str.split (Str.regexp "[ ]+") |>
                             List.map(fun x -> (int_of_string x, false))

let parse_matrices ls = 
  let rec aux (acc ,board) = function
    | [] -> List.rev((List.rev board)::acc)
    | hd::tl -> if hd="" 
      then match board with 
        | [] -> aux (acc, []) tl 
        | _ -> aux ((List.rev board)::acc, []) tl 
      else 
        aux (acc, hd::board) tl in
  ls |> 
  aux ([],[]) |> 
  List.map (List.map parse_row) 

let parse_input = 
  readFile >>
  (fun (hd::tl) -> (parse_reading hd, parse_matrices tl))

let markRow (n:int) (r:row) = List.map (fun (x, b) -> if x=n then (x, true) else (x, b)) r

let checkRow (r:row) : bool = List.fold_left (fun acc (_, b) -> acc && b) true r;;

let markMatrix n = List.map (markRow n)

(* let checkRow (ls:row) = List.fold_left (fun acc (_, b) -> b&&acc ) true ls
   let marke (m:matrix) (n:int) = 
   List.map () *)

let () = "./input" |>
         parse_input |>
         sexp_of_problem |>
         Core.Sexp.to_string |>
         print_endline