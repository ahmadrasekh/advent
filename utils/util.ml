let readFile file f = 
  let lines = Core_kernel.In_channel.read_lines file in
  List.map f lines

let sum = function
  | [] -> []
  | hd::[] -> hd
  | hd::tl -> List.fold_left (fun xs ys -> List.map2 (+) xs ys) hd tl


let flipBits = 
  String.map (function | '0' -> '1' | '1' -> '0' | _ -> failwith "non binary char")

let binToDec str = int_of_string (String.concat "" ["0B"; str])