#use "../lib/utils.ml"

(* "a((aa|bb)(ab|ba)|(ab|ba)aa|bb)b" *)
(* "a((aa|bb)(ab|ba)|(ab|ba)(aa|bb))b" *)
(* let pattern = Str.regexp "a((aa|bb)(ab|ba)|(ab|ba)aa|bb)b" *)
(* each string:
split_str_at_delims " " : gives a list of string
map over list: repplace string: replace str (find definition of str)
(flatten list)
go till reach fixpoint
concat using " "
make regex
find matchs *)

(* let replace ptrn str = Str.global_replace (Str.regexp ptrn) (Printf.sprintf "( %s )" str) *)

let parse_input = 
  read_file >>
  split_list_at_delim "" [] >>
  fun (rules, inputs) ->
    rules |> 
     List.map (split_str_to_tuple ": ") |> 
     List.sort (fun x y -> compare (int_of_string (fst x)) (int_of_string (fst y))),
     inputs 


let replace ptrn str = Str.global_replace (Str.regexp ptrn) (Printf.sprintf "%s" str)

let format = 
  remove [" "] 
  (* >>
  replace "(" "\(" >>
  replace ")" "\)" >>
  replace "|" "\|"  *)

let expand db s = 
  if Hashtbl.mem db s then 
    let found = Hashtbl.find db s in 
    if (String.length found)=1 then found else 
   Printf.sprintf "( %s )" (replace s found s )
  else s

let mkRegex rules  = 
  (* let start = snd (List.hd rules) in *)
  let start = "42" in
  let db = Hashtbl.create (List.length rules) in

  let rec aux acc =
    let curr = acc |> 
               split_str_at_delims [" "] |>
               List.map (expand db) |>
               String.concat " " in
    if curr = acc then 
       acc
      |> format   
      (* |> Str.regexp  *)
    else 
      aux curr in
    
  List.iter (fun (k, v) -> Hashtbl.add db k ( remove ["\""] v) ) rules;
  (* Hashtbl.replace db "8" "[ 42 ]+"; *)
  (* Hashtbl.replace db "11" "[ 42 ][ 31 ]"; *)
  aux start




let _ = 
  "input" |>
  parse_input |> 
  fun (rules, inputs) -> 
  mkRegex rules
    (* List.map (fun s ->
       Str.string_match (mkRegex rules) s 0 && ((String.length s) = (String.length (Str.matched_string s)))
    ) inputs |>
    count_in_list true *)

    "((b(a(bb|ab)|b((a|b)(a|b)))|a(b(bb)|a(bb|a(a|b))))b|(((aa|ab)a|(bb)b)b|(((a|b)a|bb)a)a)a)"