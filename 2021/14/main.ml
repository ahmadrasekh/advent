open Utils.Util
open Sexplib.Std

(* type t = (string list) [@@deriving sexp_of] *)
(* type t = (char * int) array [@@deriving sexp_of] *)
type t = (string * int ref) list [@@deriving sexp_of]

(* let parse_line str =
   let f x y = (x, y) in
   Scanf.sscanf str "%i,%i" f

   let parse_input () = "./input" |>
                     read_file |>
                     List.map parse_line *)

let rules_ = function
  | "CH" -> ["CB"; "BH"]
  | "HH" -> ["HN"; "NH"]
  | "CB" -> ["CH"; "HB"]
  | "NH" -> ["NC"; "CH"]
  | "HB" -> ["HC"; "CB"]
  | "HC" -> ["HB"; "BC"]
  | "HN" -> ["HC"; "CN"]
  | "NN" -> ["NC"; "CN"]
  | "BH" -> ["BH"; "HH"]
  | "NC" -> ["NB"; "BC"]
  | "NB" -> ["NB"; "BB"]
  | "BN" -> ["BB"; "BN"]
  | "BB" -> ["BN"; "NB"]
  | "BC" -> ["BB"; "BC"]
  | "CC" -> ["CN"; "NC"]
  | "CN" -> ["CC"; "CN"]
  |  _ -> failwith "no match"

let test n template = 
  let hash = Hashtbl.create 100 in
  (* initialisation step *)
  let template_list = template |>
                      Core.String.to_list |>
                      List.map Core.String.of_char |>
                      pairs_from_list |>
                      List.map (String.concat "") in 
  List.iter (fun pair -> match Hashtbl.find_opt hash pair with
      | Some count -> incr count
      | None -> Hashtbl.add hash pair (ref 1)
    ) template_list;

  let rec aux n = 
    if n > 0 then
      hash |> Hashtbl.to_seq |> List.of_seq
    else 
      hash |> Hashtbl.to_seq |> List.of_seq

let template_ = "NNCB"

let template = "NBOKHVHOSVKSSBSVVBCS"

let rules = function
  | "SN" -> "SH"
  | "KP" -> "KO"
  | "CP" -> "CV"
  | "FN" -> "FP"
  | "FV" -> "FS"
  | "HO" -> "HS"
  | "NS" -> "NN"
  | "OP" -> "OC"
  | "HC" -> "HS"
  | "NP" -> "NB"
  | "CF" -> "CV"
  | "NN" -> "NO"
  | "OS" -> "OF"
  | "VO" -> "VV"
  | "HK" -> "HN"
  | "SV" -> "SV"
  | "VC" -> "VV"
  | "PH" -> "PK"
  | "NH" -> "NO"
  | "SB" -> "SN"
  | "KS" -> "KV"
  | "CB" -> "CH"
  | "SS" -> "SP"
  | "SP" -> "SH"
  | "VN" -> "VK"
  | "VP" -> "VO"
  | "SK" -> "SV"
  | "VF" -> "VC"
  | "VV" -> "VB"
  | "SF" -> "SK"
  | "HH" -> "HK"
  | "PV" -> "PV"
  | "SO" -> "SH"
  | "NK" -> "NP"
  | "NO" -> "NC"
  | "ON" -> "OS"
  | "PB" -> "PK"
  | "VS" -> "VH"
  | "SC" -> "SP"
  | "HS" -> "HP"
  | "BS" -> "BP"
  | "CS" -> "CP"
  | "VB" -> "VV"
  | "BP" -> "BK"
  | "FH" -> "FO"
  | "OF" -> "OF"
  | "HF" -> "HF"
  | "FS" -> "FC"
  | "BN" -> "BO"
  | "NC" -> "NF"
  | "FC" -> "FB"
  | "CV" -> "CV"
  | "HN" -> "HC"
  | "KF" -> "KK"
  | "OO" -> "OP"
  | "CC" -> "CS"
  | "FF" -> "FC"
  | "BC" -> "BP"
  | "PP" -> "PF"
  | "KO" -> "KV"
  | "PC" -> "PB"
  | "HB" -> "HH"
  | "OB" -> "ON"
  | "OV" -> "OS"
  | "KH" -> "KB"
  | "BO" -> "BB"
  | "HV" -> "HP"
  | "BV" -> "BK"
  | "PS" -> "PF"
  | "CH" -> "CC"
  | "SH" -> "SH"
  | "OK" -> "OV"
  | "NB" -> "NK"
  | "BF" -> "BS"
  | "CO" -> "CO"
  | "NV" -> "NH"
  | "FB" -> "FK"
  | "FO" -> "FC"
  | "CK" -> "CP"
  | "BH" -> "BB"
  | "OH" -> "OF"
  | "KB" -> "KN"
  | "OC" -> "OK"
  | "KK" -> "KO"
  | "CN" -> "CH"
  | "FP" -> "FK"
  | "VH" -> "VK"
  | "VK" -> "VP"
  | "HP" -> "HS"
  | "FK" -> "FF"
  | "BK" -> "BH"
  | "KV" -> "KV"
  | "BB" -> "BO"
  | "KC" -> "KF"
  | "KN" -> "KC"
  | "PO" -> "PP"
  | "NF" -> "NP"
  | "PN" -> "PS"
  | "PF" -> "PS"
  | "PK" -> "PO"
  |  _ -> failwith "no match"

let rec recurse n rules template =
  let template' = template |>
                  Core.String.to_list |>
                  List.map Core.String.of_char |>
                  pairs_from_list |>
                  List.map (String.concat "") |>
                  List.map rules |>
                  String.concat "" |>
                  (fun str -> String.concat "" [str; Core.String.of_char (String.get template ((String.length template)-1) )])  in
  if n > 0 then recurse (n-1) rules template' else template

let count_occurances str = 
  let hash = Hashtbl.create 100 in
  String.iter ( fun c -> 
      match (Hashtbl.find_opt hash c) with 
      | Some (c, count) -> Hashtbl.replace hash c (c, count+1)
      | None -> Hashtbl.add hash c (c, 1)
    ) str;
  hash |>
  Hashtbl.to_seq_values |>
  Array.of_seq |>
  (fun arr -> Array.sort (fun (_, count) (_, count') -> compare count count') arr; arr)

let () = template_ |>
         test |>
         sexp_of_t |>
         Core.Sexp.to_string |>
         print_endline 

         (* let () = template |>
                  recurse 5 rules |>
                  (* count_occurances |>
                     (fun arr -> 
                     let (_, min ) = arr.(0) in 
                     let (_, max) = arr.((Array.length arr)-1) in
                     max - min
                     ) |> *)
                  sexp_of_t |>
                  Core.Sexp.to_string |>

                  print_endline *)
         (* let () = "ahmad" |>
                  count_occurances |>
                  sexp_of_t |>
                  Core.Sexp.to_string |>
                  print_endline *)