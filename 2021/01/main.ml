open Utils.Util

let parse_input () = "./input" |>
                     read_file |>
                     List.map int_of_string

let sums ls = 
  let rec aux acc = function
    | x::(y::z::_ as tl) -> aux ((x+y+z)::acc) tl
    | _ -> acc in
  List.rev(aux [] ls)

let count ls =
  let rec aux acc = function
    | x::(y::_ as tl) -> if y>x then aux (acc+1) tl else aux acc tl
    | _ -> acc in
  aux 0 ls

let part_01 input = count input
let part_02 input = count (sums input)

let () = parse_input () |>
         fun input -> Printf.printf "part 1: %d\n part 2: %d\n" (part_01 input) (part_02 input)
