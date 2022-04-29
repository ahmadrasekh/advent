#use "../lib/utils.ml"
#use "./validators.ml"


let group_passports ls =
  let rec aux curr acc = function
  | hd::tl -> (if hd = "" then aux [] (curr::acc) tl else aux (hd::curr) acc tl)
  | [] -> (curr::acc) in
  aux [] [] ls
  
let mk_raw_pair s = Scanf.sscanf s "%3s:%s" (fun k v -> (k, v))
  
let mk_passport =  
  List.map (String.split_on_char ' ') >> 
  List.flatten >>
  List.map mk_raw_pair
    
let parse_input = 
  read_file >>
  group_passports >>
  List.map mk_passport

let validate_a ls =
  (List.mem_assoc "byr" ls) && (List.mem_assoc "iyr" ls) && 
  (List.mem_assoc "eyr" ls) && (List.mem_assoc "hgt" ls) && 
  (List.mem_assoc "hcl" ls) && (List.mem_assoc "ecl" ls) &&
  (List.mem_assoc "pid" ls)  

  let validate_b ls =
  (validate_a ls ) && 
  (validate_byr (List.assoc "byr" ls)) && (validate_iyr (List.assoc "iyr" ls)) &&
  (validate_eyr (List.assoc "eyr" ls)) && (validate_hgt (List.assoc "hgt" ls)) &&
  (validate_hcl (List.assoc "hcl" ls)) && (validate_ecl (List.assoc "ecl" ls)) &&
  (validate_pid (List.assoc "pid" ls))

let _ =
  "input" |>
  parse_input |>
  List.map validate_b |>
  count_in_list true
