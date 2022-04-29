#use "../lib/utils.ml"


let parse_input = 
  read_file >>
  List.map (fun str -> Scanf.sscanf str "%c%i" (fun c v -> (c,v)))

(* 
north (90) is positive y
east (0) is positive x
south (270) is negative y 
west (180) is negative x
*)

type direction = North | East | South | West

(* let changeDir current (direction, angle) = match direction with 
     | 'L' -> (360+current+angle) mod 360
     | 'R' -> (360+current-angle) mod 360 *)

(* let eval ((x,y), (current_direction)) = function
  | ('L', v) as action -> ((x,y), (changeDir current_direction action))
  | ('R', v) as action ->  ((x,y), (changeDir current_direction action))
  | ('N', v) -> ((x, y+v), current_direction)
  | ('S', v) -> ((x, y-v), current_direction)
  | ('E', v) -> ((x+v, y), current_direction)
  | ('W', v) -> ((x-v, y), current_direction)
  | ('F', v) -> match current_direction with 
    | 0 -> ((x+v,y), current_direction)
    | 90 -> ((x,y+v), current_direction)
    | 180 -> ((x-v,y), current_direction)
    | 270 -> ((x,y-v), current_direction) *)

let rotate amount (x,y) = match amount with 
  | ('L',90) -> (-y, x)
  | ('L',180) -> (-x, -y)
  | ('L',270) -> (y, -x)
  | ('R', 90) ->  (y, -x)
  | ('R', 180) -> (-x, -y)
  | ('R', 270) -> (-y, x)


let eval ((x,y), (wx, wy)) = function
  | ('L', v) as amount -> ((x,y), rotate amount (wx, wy))
  | ('R', v)  as amount ->  ((x,y), rotate amount (wx, wy))
  | ('N', v) -> ((x, y), (wx, wy+v))
  | ('S', v) -> ((x, y), (wx, wy-v))
  | ('E', v) -> ((x, y), (wx+v, wy))
  | ('W', v) -> ((x, y), (wx-v, wy))
  | ('F', v) -> let (wx', wy') = (wx*v, wy*v) in ((x+wx', y+wy'), (wx, wy))


let _ =
  "input" |>
  parse_input |>
  List.fold_left (fun acc action -> eval acc action ) ((0,0), (10, 1)) |>
  (fun ((x, y), _) -> (abs x) + (abs y))

