open Utils.Util
open Sexplib.Std

type t = (int * int) list [@@deriving sexp_of]

let parse_line str =
  let f x y = (x, y) in
  Scanf.sscanf str "%i,%i" f

let parse_input () = "./input" |>
                     read_file |>
                     List.map parse_line

let folds = [
  (655, 0);
  (0, 447);
  (327, 0);
  (0, 223);
  (163, 0);
  (0, 111);
  (81, 0);
  (0, 55);
  (40, 0);
  (0, 27);
  (0, 13);
  (0, 6 )
]

let fold_x x = 
  List.filter_map (
    fun (i, j) ->
      if i > x then 
        let i' = (2*x)-i in 
        if i' >= 0 then Some(i', j)
        else None 
      else Some (i,j)
  ) 

let fold_y y = 
  List.filter_map (
    fun (i, j) ->
      if j > y then 
        let j' = (2*y)-j in 
        if j' >= 0 then Some(i, j')
        else None 
      else Some (i,j)
  )

let rec fold  instructions input = match instructions with
  | [] -> input
  | (0, y)::tl -> let input' = fold_y y input in fold tl input'
  | (x, 0)::tl -> let input' = fold_x x input in fold tl input'

let plot (input:t) =
  let (x, y) = input |> 
               List.split |>
               fun (xs, ys) -> 
               (
                 xs |> List.sort_uniq compare |> List.rev |> List.hd |> (+) 1,
                 ys |> List.sort_uniq compare |> List.rev |> List.hd |> (+) 1
               ) in
  Array.make y (fun () -> (Array.make x '.')) |> 
  Array.map (fun x -> x () ) |> 
  (fun canvas -> 
     List.iter (fun (x, y) -> canvas.(y).(x) <- '0') input;
     canvas |>
     Array.map (Array.to_seq >> String.of_seq) |>
     Array.iter print_endline)

let () = parse_input () |>
         fold folds |>
         List.sort_uniq compare |> 
         plot
(* sexp_of_t |>
   Core.Sexp.to_string |>
   print_endline *)

