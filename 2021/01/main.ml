open Utils

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

let input = Util.readFile "./input.txt" int_of_string

let result01 = count input
let result02 = count (sums input)

let () = Printf.printf "part 1: %d\n part 2: %d\n" result01 result02
