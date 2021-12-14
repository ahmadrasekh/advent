open Utils

let parse_line str =  Str.split (Str.regexp "") str

let calcFrequency f input = 
  let tmp = input |>
            List.map (fun x -> x |> parse_line |> List.map(int_of_string)) in
  let max = tmp |> 
            List.length |> 
            Float.of_int |> 
            (fun x -> x /. 2.) |> 
            Float.ceil |> 
            Float.to_int in
  let freq = tmp |> 
             Util.sum |> 
             List.map (fun x -> if f x max then "1" else "0") |>
             String.concat ""  in
  freq

let rec aux f index input = match input with
  | []
  | _::[] -> input
  | _ -> let freq = calcFrequency f input in
    if index < (String.length freq) then 
      aux f (index+1) (List.filter (fun bin -> Util.sameBinAt index freq bin) input) 
    else input 

let lambda_01 input =
  let tmp = input |>
            List.map (fun x -> x |> parse_line |> List.map(int_of_string)) in
  let max = input |> 
            List.map (fun x -> x |> parse_line |> List.map(int_of_string))|> 
            List.length |>
            (fun x -> x / 2) in
  let sums = tmp |> Util.sum in
  let gamma = String.concat "" (List.map (fun x -> if x>=max then "1" else "0") sums) in
  let epsilon = Util.flipBits gamma in
  (Util.binToDec gamma ) * (Util.binToDec epsilon)


let lambda_02 input = 
  let x = input |> aux (>=) 0 |> List.hd |> Util.binToDec in
  let y = input |> aux (<) 0 |> List.hd |> Util.binToDec in
  x*y


let () =  "./input" |>
          Util.readFile |>
          (*  *)
          lambda_01 |>
          (*  *)
          (* lambda_02 |> *)
          (*  *)
          string_of_int |>
          print_endline