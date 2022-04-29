#use "../lib/utils.ml"

let parse_input = 
  read_file  >>
  List.map (String.to_seq >> List.of_seq)

let get_every_nth_y y = 
  let rec aux i acc = function
    | [] -> acc
    | hd::tl -> (if (i mod y)=0 then 
                  aux (i+1) (hd::acc) tl
                else 
                  aux (i+1) acc tl) in
(aux 0 []) >> List.rev >> List.tl

let get_every_nth_x x = 
      List.fold_left (
      fun (acc, i) ls' -> 
       let found = List.nth ls' ((i*x)mod(List.length ls')) in
       (found::acc, i+1)
    ) ([], 1) >> fst >> List.rev

let traverse_slope (r, d) = 
  get_every_nth_y d >> 
  get_every_nth_x r >>
  List.filter (( = ) '#') >>
  List.length

let _ =
  let  slopes = [(1,1); (3, 1); (5, 1); (7,1);  (1, 2)] in
  List.map (
    fun slope ->
      (parse_input "input") |> 
      traverse_slope slope
  ) slopes |>
  list_multiply