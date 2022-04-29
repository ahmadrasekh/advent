(* ================================================== *)
(* composition etc *)
(* ================================================== *)
let (>>) f g x = g(f(x));;

let id = fun x -> x
(* ================================================== *)
(* io *)
(* ================================================== *)
let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := (input_line chan) :: !lines
    done;
    !lines (* to keep the type checker happy *)
  with End_of_file -> close_in chan;
                      List.rev !lines

let read_file_f f filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := (chan |> input_line |> f)  :: !lines
    done;
    !lines (* to keep the type checker happy *)
  with End_of_file -> close_in chan;
                      List.rev !lines
(* ================================================== *)
(* input parsers *)
(* ================================================== *)
let group delim =
let rec aux curr acc = function
| hd::tl -> (if hd = delim then aux [] (curr::acc) tl else aux (hd::curr) acc tl)
| [] -> (curr::acc) in
aux [] []


(* ================================================== *)
(* lists *)
(* ================================================== *)
let count_in_list x ls = 
  List.fold_left (fun acc elem -> if elem=x then acc+1 else acc) 0 ls

let rec split_list_at_index n acc l =
  if n = 0 then 
    (List.rev acc, l) 
  else
    match l with
      | [] -> (List.rev acc, [])
      | h :: t -> split_list_at_index (n-1) (h :: acc) t

let rec split_list_at_delim str acc = function 
  | hd::tl -> if hd=str then (List.rev acc, tl)
              else split_list_at_delim str (hd::acc) tl
  | _ -> (List.rev acc,[])

(* ================================================== *)
(* strings                                            *)
(* ================================================== *)
let white_space = "[ \n\r\x0c\t]+"
let strip_last_char  = function
  | "" -> ""
  | _ as str -> String.sub str 0 ((String.length str) - 1)
  
let explode = 
  String.to_seq >>
  List.of_seq

let count_in_string ch = 
  explode >>
  count_in_list ch

let string_fold f a str = 
  List.fold_left f a (explode str)  

let remove words = 
  let bundle = String.concat "\\|" words in
  let r = Printf.sprintf "%s*\\(%s\\)" white_space bundle in
  Str.global_replace (Str.regexp r) ""

let split_str_at_delims delims = 
  let bundle = String.concat "\\|" delims in
  let d = Printf.sprintf "%s*\\(%s\\)%s*" white_space bundle white_space in
  Str.split (Str.regexp d) 
  (* >>
  List.map String.trim *)

let split_str_to_tuple delim str =
  match split_str_at_delims [delim] str with
  | k::v::[] -> (k,v)
  | _ -> failwith "string contains multiple instance of delim"

(* ================================================== *)
(* ints *)
(* ================================================== *)
let in_range (min, max) x =
  if min < max then min <= x && x <= max
  else failwith "range format: (min, max) with min < max"

let list_multiply = List.fold_left (fun acc x -> acc * x) 1

let list_add = List.fold_left (fun acc x -> acc + x) 0

(* ================================================== *)
(* logic *)
(* ================================================== *)
let int_of_bool = function
| true -> 1
| false -> 0

let bool_of_int = function
| 0 -> false
| _ -> true

(* ================================================== *)
(* ints *)
(* ================================================== *)

let ( -- ) x y =
  let rec aux n acc =
    if n < x then acc else aux (n-1) (n :: acc)
  in aux y [] ;;