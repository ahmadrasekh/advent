#use "../lib/utils.ml"

exception FailedOn of int

let parse_input = 
  read_file_f int_of_string

(* think closing in from the ends *)
let validate x xs = 
  let forward = List.sort compare xs in
  let backward = List.rev forward in 
  let rec aux = function 
    | (f::fs),(b::bs) -> let candid = b+f in 
                        if candid=x then (f,b) else
                        if candid>x then aux ((f::fs),(bs)) else
                        aux ((fs),(b::bs))
    | _ -> raise (FailedOn x) in
  aux (forward,backward)

(* think sliding window *)
let find_contiguous n ls = 
  let rec aux acc ls = 
    if (list_add acc)=n then 
      acc
    else if (list_add acc)<n then 
      aux (acc@[(List.hd ls)]) (List.tl ls)
    else
      aux (List.tl acc) ls in
  aux [] ls


  let rec test last_n current next_all = 
  try 
    ignore(validate current last_n);
    test ((List.tl last_n)@[current]) (List.hd next_all) (List.tl next_all)
  with
    | FailedOn x -> x
  
let hhd ls = List.nth ls ((List.length ls)-1)

let rec split_list_at_index n acc l =
  if n = 0 then (List.rev acc, l) else
  match l with
  | [] -> (List.rev acc, [])
  | h :: t -> split_at2 (n-1) (h :: acc) t;;

let _ = 
  let input = "input" |> parse_input  in
  input |> 
  (fun ls -> 
    let (preamble, (hd::tl)) = split_list_at_index 25 [] ls in
    test preamble hd tl
  ) |>
  (fun x -> find_contiguous x input) |>
  List.sort compare |>
  fun ls -> (List.hd ls)+(hhd ls)


