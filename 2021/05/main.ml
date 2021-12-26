open Utils.Util
open Sexplib.Std
open Utils.LinearAlgebra



type point = int * int [@@deriving sexp_of];;
type vector = int list [@@deriving sexp_of];;
type matrix = vector list [@@deriving sexp_of];;
type segment = point * point [@@deriving sexp_of];;
type pointSequence = point list [@@deriving sexp_of];;
type pointWithCounter = point * int [@@deriving sexp_of]
type t = segment list [@@deriving sexp_of];;
type paritioned = t * t * t [@@deriving sexp_of]
let parse_line str = 
  let f a b c d = ((a,b),(c,d)) in
  Scanf.sscanf str "%i,%i -> %i,%i" f

let parse_input file : segment list= file |>  
                                     read_file |>
                                     (List.map parse_line)

(* let parse_reading str = str |> 
                        Str.split (Str.regexp " -> ") |>
                        List.map(int_of_string)  *)

let input : t = [
  ((0,9), (5,9));
  ((8,0), (0,8));
  ((9,4), (3,4));
  ((2,2), (2,1));
  ((7,0), (7,4));
  ((6,4), (2,0));
  ((0,9), (2,9));
  ((3,4), (1,4));
  ((0,0), (8,8));
  ((5,5), (8,2));
];;

(* filter horizontal or vertical *)
let partition (input:t ) : (t * t * t) = 
  let rec aux ((hs, vs, ds) as acc) = function
    | [] -> acc
    | ( ((x1, y1) ,(x2, y2))as hd)::tl when x1=x2 -> aux (hs, hd::vs, ds) tl 
    | ( ((x1, y1) ,(x2, y2))as hd)::tl when y1=y2 -> aux (hd::hs, vs, ds) tl 
    | hd::tl -> aux (hs, vs, hd::ds) tl
  in aux ([],[], []) input

(* e readint list [@@deriving sexp_of]
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
   read_file >>
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
         print_endline *)
let tmp  = "./input" |>
           parse_input |>
           partition 
           |>
           (fun (h, v, d) -> (List.map mkSegmentH h, List.map mkSegmentV v, List.map mkSegmentD d)) |>
           (fun (x, y, z) -> List.concat [List.flatten x; List.flatten y; List.flatten z]) |> 
           count_unique_elements |>
           List.fold_left (fun acc (_, i) -> if i>1 then acc+1 else acc) 0


let () = tmp |> 
         (* sexp_of_paritioned |>  *)
         (Core.Int.sexp_of_t) |> 
         Core.Sexp.to_string |>
         print_endline