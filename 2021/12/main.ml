open Utils.Util
open Sexplib.Std

(** 
   find number of distict paths such that on any path
   small caves are visited at most once
*)
(**
   reduce all nodes labesl to a single letter
   path is then just a string
   to check if a path has a node, check if char in string
*)

(* 
A finite automaton is a 5-tuple (Q, ,  , q0, F), where
1. Q is a finite set called the states,
2.   is a finite set called the alphabet,
3.   : Q ×  −!Q is the transition function,1
4. q0 2 Q is the start state, and
5. F   Q is the set of accept states.2 
*)

(* 
A context-free grammar is a 4-tuple (V, ,R, S), where
1. V is a finite set called the variables,
2.   is a finite set, disjoint from V , called the terminals,
3. R is a finite set of rules, with each rule being a variable and a
string of variables and terminals, and
4. S 2 V is the start variable.
 *)

type fsa = {
  start: char;
  terminal: char;
  alphabet: char list;
  rules : (char, (char list)) Hashtbl.t
}

let verify_fsa_input ~start:s ~terminal:t ~alphabet:a ~rules:r = 
  (List.mem s a) &&
  (List.mem t a) &&
  List.fold_left (fun acc (x, ys) -> 
      acc &&
      (List.mem x a) &&
      List.fold_left (fun acc x -> acc && (List.mem x a)) true ys
    ) true r 

let mk_fsa ~start:s ~terminal:t ~alphabet:a ~rules:r = 
  if (verify_fsa_input ~start:s ~terminal:t ~alphabet:a ~rules:r) then 
    {
      start=s;
      terminal=t;
      alphabet=a;
      rules = List.to_seq r |> Hashtbl.of_seq
    }
  else failwith "mk_error"

let test = mk_fsa 
    ~start:'s' 
    ~terminal:'e' 
    ~alphabet:['A'; 'b'; 'c'; 'd'; 'e'; 's']
    ~rules:[
      ('s', ['A'; 'b'; 'c']);
      ('A', ['c'; 'b'; 'e']);
      ('b', ['d'; 'e'; 'A']);
      ('c', ['A']);
      ('d', ['b']);
    ] 

(** 
   first char is start
   last char is terminal
   and for all consecutive pairs (a, b) : List.mem b rules.(a)

*)
let check_path (fsa:fsa) (path:string) : bool = 
  let path = path |> String.to_seq |> List.of_seq  in
  let rec aux (b, prev) rest = match rest with
    | [] -> b
    | cur::[] -> 
      begin 
        match Hashtbl.find_opt fsa.rules prev  with 
        | Some ls ->  b && (List.mem cur ls) && (cur = fsa.terminal)
        | None -> false
      end 
    | cur::tl -> 
      begin 
        match Hashtbl.find_opt fsa.rules prev  with 
        | Some ls -> aux ((b && (List.mem cur ls)), cur) tl
        | None -> false
      end in
  match path with 
  | [] | _::[] -> false
  | hd::tl -> hd = (fsa.start) && (aux (true, hd) tl)


type t = string list [@@deriving sexp_of]

let str_hd str = try Some (String.get str 0 |> Core.Char.to_string) with _ -> None
let str_has = Core.String.contains

let parse_lines lines = lines |>
                        List.map (Str.split (Str.regexp "-")) |>
                        List.fold_left  (fun acc (a::b::_) -> (a,b)::(b,a)::acc) [] |>
                        List.filter (function  (_, "s") | ("e", _) -> false | _ -> true) |> 
                        List.fold_left ( fun acc (x,y)->
                            match List.assoc_opt x acc with
                            | Some ls -> ls := (y::(!ls)); acc
                            | None -> (x, ref (y::[]))::acc
                          ) [] |>
                        List.map (fun (a, ref) -> (a, !ref))

let parse_input () = "./input" |>
                     read_file |>
                     parse_lines

let is_lower s = 
  Core.Char.of_string s |>
  int_of_char |>
  fun x -> (x >= 97 ) && (x <= 122)

let is_lower_char s = 
  int_of_char s |>
  fun x -> (x >= 97 ) && (x <= 122)

let collect_conseq_dupes ls = 
  let (accs, acc) = List.fold_left (fun (acc, tmp) a -> 
      match Core.List.hd tmp with 
      | Some tmp_hd -> if tmp_hd = a then (acc, a::tmp) else (tmp::acc, a::[])
      | None -> (acc, a::tmp) ) ([], []) ls in 
  (match acc with | [] -> accs | _ -> acc::accs) |> List.filter (fun ls -> (List.length ls)>1 )

let test_test str = 
  Core.String.to_list str |>
  List.filter is_lower_char |>
  List.sort compare |>
  collect_conseq_dupes |>
  List.length |>
  (fun x -> x<2)




let test path = 
  let is_lower s = 
    int_of_char s |>
    fun x -> (x >= 97 ) && (x <= 122) in
  let has_dups (hd::tl) = 
    Core.List.fold_until
      ~init: hd
      ~f:(fun prev next -> 
          if prev = next 
          then  Base.Continue_or_stop.Stop (true)
          else Base.Continue_or_stop.Continue (next))
      ~finish:(fun x -> false) tl in
  let p = path |>
          Core.String.to_list |>
          List.sort compare |>
          List.filter (fun x -> (x<>'s')&&(x<>'e') ) |>
          List.filter is_lower |>
          function [] | _::[] -> false | _ as ls -> has_dups ls in
  p

let count node = Core.String.count  ~f:((=) node)

let is_illegal path node =
  (is_lower node) && (Core.String.mem path (Core.Char.of_string node))

let is_illegal_ path node =
  (is_lower node) && (count (Core.Char.of_string node) path)>=2

let mkPath partial_paths assoc  = 
  partial_paths |>
  List.map (fun input -> 
      let sO = str_hd input in
      match sO with 
      | Some s ->
        begin 
          match List.assoc_opt s assoc with
          | Some next_list -> List.map (fun next -> 
              if (is_illegal_ input next) then input else  next^input
            ) next_list
          | None -> [input]
        end
      | None -> []) |>
  List.flatten |>
  List.sort_uniq (String.compare)

let recurse n assoc =
  let rec aux counter partials = 
    if (counter>0) then aux (counter-1) (mkPath partials assoc) else partials in
  aux n ["s"]

let recurse_  assoc =
  let rec aux partials = 
    let partials' = mkPath partials assoc in
    if (List.length partials) = (List.length partials') then partials' else  (mkPath partials' assoc) in
  aux  ["s"]

let () = 
  let complete_path = Str.regexp "s\\([A-Za-df-rt-z]+\\)e" in
  parse_input () |>
  recurse 50|>
  List.map Core.String.rev |>
  List.filter (fun str -> Str.string_match complete_path str 0) |>
  List.filter test_test |>
  List.length |>
  (* sexp_of_t |>
     Core.Sexp.to_string |> *)
  string_of_int |>
  print_endline
