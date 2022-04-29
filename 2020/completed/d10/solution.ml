#use "../lib/utils.ml"


let parse_input = read_file_f int_of_string

let solve ls =
  let rec aux (x,y,z) cur = function
    | [] -> (x+1,y,z+1)
    | hd::tl -> if (hd-cur) = 1 then  aux (x+1,y,z) hd tl
                else if (hd-cur) = 2 then  aux (x,y+1,z) hd tl
                else if (hd-cur) = 3 then  aux (x,y,z+1) hd tl
                else failwith "incorrect sequence" in
  aux (0,0,0) (List.hd ls) (List.tl ls)

let in_range x y = (y-x)<3 

let fact n =
  let rec aux x acc =
    if x=n then acc*x else
    aux (x+1) (x*acc) in
  aux 1 1
      

let partition ls = 
  List.fold_left (fun (p::partitions) x -> 
    if in_range (List.hd p) x then ((x::p)::partitions) else ([x]::(p::partitions))
  ) [[List.hd ls]] (List.tl ls)

let _ = 
  "input" |>
  parse_input |>
  List.sort compare |>
  partition |>
  List.map List.rev |>
  List.rev |>
  List.map (List.length) |>
  List.map (fun x -> match x with 
  | 1 | 2 -> 1
  | 3 -> 2
  | 4 -> 4
  | 5 -> 7) |>
  list_multiply
(*  List.filter (fun partition -> (List.length partition)>2 ) *)
  (* |> (fun ls -> (list_add ls) / (List.length ls)) *)
