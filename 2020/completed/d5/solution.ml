#use "../lib/utils.ml"

let parse_input = 
  read_file >>
  List.map String.to_seq >>
  List.map List.of_seq >>
  List.map (split_list_at_index 7 [])

let calc =
  let rec aux (lower, upper) = function
    | 'F'::[] | 'L'::[] -> lower
    | 'B'::[] | 'R'::[] -> upper
    | 'F'::tl | 'L'::tl -> aux (lower, (lower+upper)/2) tl
    | 'B'::tl | 'R'::tl -> aux (((lower+upper)/2)+1, upper) tl in
  aux

let calc_row = calc (0,127)
let calc_col = calc (0, 7)

let rec find = function
  | x::y::tl -> if (x+1)=y then find (y::tl) else x+1
  | _ -> failwith "debug"

let _ = 
  "input" |>
  parse_input |>
  List.map (fun (r, c) -> (calc_row r, calc_col c)) |>
  List.map (fun (r, c) -> (r*8)+c) |>
  List.sort compare |>
  (* List.hd *)
  find