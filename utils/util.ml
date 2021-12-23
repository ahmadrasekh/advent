
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

let count_unique_elements list =
  let counter = Hashtbl.create 10000 in
  let update_counter x = 
    if Hashtbl.mem counter x then
      let current_count = Hashtbl.find counter x in
      Hashtbl.replace counter x (succ current_count)
    else
      Hashtbl.replace counter x 1
  in
  List.iter update_counter list;
  Hashtbl.to_seq counter
  |> List.of_seq

let memoize f =
  let h = Hashtbl.create 100 in 
  let rec fct x = match (Hashtbl.find_opt h x ) with
    | Some t -> t
    | None -> (let t = f fct x in
               Hashtbl.add h x t;
               t) in fct

let sortstr s = 
  String.to_seq s |> List.of_seq |> List.sort Char.compare |> List.to_seq |> String.of_seq;;
