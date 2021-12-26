open Utils.Util

let parse_line str =
  let f x y = (x, y) in
  Scanf.sscanf str "%s %i" f

let parse_input () = "./input" |>
                     read_file |>
                     List.map parse_line

let part_01 input = 
  let (h, v) = List.fold_left (fun (x, y) (command, z) -> match command with
      | "forward" -> (x+z, y)
      | "up" -> (x, y-z)
      | "down" -> (x, y+z)
      | _ -> (x, y)
    ) (0, 0) input in
  h * v

let part_02 input = 
  let (h, v, aim) = List.fold_left (fun (x, y, a) (command, n) -> match command with
      | "down" -> (x, y, a+n)
      | "up" -> (x, y, a-n)
      | "forward" -> (x+n, y+(a*n), a)
      | _ -> (x, y, a)
    ) (0,0,0) input in
  h*v

let () = parse_input () |>
         fun input -> Printf.printf "solution #1: %d\nsolution #2: %d" (part_01 input) (part_02 input)