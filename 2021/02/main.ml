open Utils

let parse_line str =
  let f x y = (x, y) in
  Scanf.sscanf str "%s %i" f

let input = Util.readFile "./input" parse_line

let (h, v) = List.fold_left (fun (x, y) (command, z) -> match command with
    | "forward" -> (x+z, y)
    | "up" -> (x, y-z)
    | "down" -> (x, y+z)
    | _ -> (x, y)
  ) (0, 0) input

let (h', v', aim) = List.fold_left (fun (x, y, a) (command, n) -> match command with
    | "down" -> (x, y, a+n)
    | "up" -> (x, y, a-n)
    | "forward" -> (x+n, y+(a*n), a)
    | _ -> (x, y, a)
  ) (0,0,0) input

let () = Printf.printf "solution #1: %d\nsolution #2: %d" (h*v) (h'*v')