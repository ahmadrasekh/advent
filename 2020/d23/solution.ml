#use "../lib/utils.ml"
module A = Res.Array

let input = [3;2;6;5;1;9;4;7;8]
(* let input = [3;8;9;1;2;5;4;6;7] *)
let max = 10
let min = 1

let rec findNext x tail =
  if (x-1) < min then findNext (max+1) tail else
  if List.mem (x-1) tail then (x-1) else
  findNext (x-1) tail

(* head is always current
pickup is always 3 to its right
destination is always in what remains
then recurse on next:: *)

let append l1 l2 =
  let rec loop acc l1 l2 =
    match l1, l2 with
    | [], [] -> List.rev acc
    | [], h :: t -> loop (h :: acc) [] t
    | h :: t, l -> loop (h :: acc) t l
    in
    loop [] l1 l2

let solve (current::a::b::c::tail) = 
  let next = findNext current tail in
  let (x,y) = split_list_at_delim next [] tail in
  x@[next]@[a;b;c]@y@[current]

let rec findNext' x tail =
  if (x-1) < min then findNext (max+1) tail else
  try 
    A.find_index (fun y -> y=(x-1)) tail 4
  with 
    _ -> findNext (x-1) tail
    
let solve' arr =
  let current = A.get arr 0
  and a =  A.get arr 1
  and b =  A.get arr 2
  and c =  A.get arr 3 
  and tail = in
  let next_index = findNext current tail in
  let (x,y) = split_list_at_delim next [] tail in
  x@[next]@[a;b;c]@y@[current]


    
let rec solve_n i ls =
    if i=0 then ls else
    solve_n (i-1) (solve ls)
    

let main i =
  (* let input' = input @ (10 -- 1000000) in *)
  input |> 
  solve_n i
  (* fun ls -> let (x,y) = split_list_at_delim 1 [] ls in y@x *)