open Utils

(* gamma rate & epsilon rate  *)

let parse_line str =  Str.split (Str.regexp "") str

let input = Util.readFile "./input" (fun str -> List.map int_of_string (parse_line str) )
let input_ = Util.readFile "./input" (fun x -> x )

let lambda xs = 
  let max = (List.length xs)/2 in
  let sums = Util.sum xs in
  String.concat "" (List.map (fun x -> if x>=max then "1" else "0") sums)

let lambda_ xs = 
  let oxygenPattern = lambda input in
  let co2Pattern = Util.flipBits oxygenPattern in
  let rec aux pattern input index = match input with
    | []
    | _::[] -> input
    | _ -> if index < (String.length pattern) then aux pattern (List.filter (fun bin -> Util.sameBinAt index pattern bin) input) (index+1) else input in
  (aux oxygenPattern xs 0, aux co2Pattern xs 0)


(* let result = lambda input
   let result' = Util.flipBits result
   let () = Printf.printf "part 1: %d %d\n" (Util.binToDec result) (Util.binToDec result') *)

let (result) = lambda input
let (a, b) = lambda_ input_ ;;
let () = Printf.printf "part 1: %s %s\n" ((List.hd a)) ((List.hd b))