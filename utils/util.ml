let (>>) f g x = g(f(x))

let readFile file = Core_kernel.In_channel.read_lines file

let sum = function
  | [] -> []
  | hd::[] -> hd
  | hd::tl -> List.fold_left (fun xs ys -> List.map2 (+) xs ys) hd tl

let flipBits = 
  String.map (function | '0' -> '1' | '1' -> '0' | _ -> failwith "non binary char")

let binToDec str = int_of_string (String.concat "" ["0B"; str])

let sameBinAt index pattern line = 
  assert((String.length pattern) = (String.length line));
  (String.get pattern index) = (String.get line index)

let transpose x = 
  let rec aux acc = function 
    | []::_ -> acc
    | rows -> let tmp = (List.map List.hd rows) in 
      let rest = List.map List.tl rows in
      aux (tmp::acc) rest in
  aux [] x