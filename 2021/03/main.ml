open Utils

(* gamma rate & epsilon rate  *)

let parse_line str =  Str.split (Str.regexp "") str

let input = Util.readFile "./input" (fun str -> List.map int_of_string (parse_line str) )

let lambda xs = 
  let max = (List.length xs)/2 in
  let sums = Util.sum xs in
  List.map (fun x -> if x>max then "1" else "0") sums

let result = lambda input
let result' = String.concat "" result
let result'' = Util.flipBits result'
let () = Printf.printf "part 1: %d %d\n" (Util.binToDec result') (Util.binToDec result'')