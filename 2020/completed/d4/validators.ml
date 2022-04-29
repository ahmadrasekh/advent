#use "../lib/utils.ml"

(* byr (Birth Year) - four digits; at least 1920 and at most 2002. *)
let validate_byr str = try 
    Scanf.sscanf str "%i" (fun v -> in_range (1920, 2002) v)
  with Stdlib.Scanf.Scan_failure _ -> false

(* iyr (Issue Year) - four digits; at least 2010 and at most 2020. *)
let validate_iyr str = try 
    Scanf.sscanf str "%i" (fun v -> in_range (2010, 2020) v)
  with Stdlib.Scanf.Scan_failure _ -> false

(* eyr (Expiration Year) - four digits; at least 2020 and at most 2030. *)
let validate_eyr str = try
    Scanf.sscanf str "%i" (fun v -> in_range (2020, 2030) v)
  with Stdlib.Scanf.Scan_failure _ -> false

(* hgt (Height) - a number followed by either cm or in: *)
(* If cm, the number must be at least 150 and at most 193. *)
(* If in, the number must be at least 59 and at most 76. *)
let validate_hgt str = try Scanf.sscanf str "%i%2s" 
  (fun v u -> 
      match u with
        | "cm" -> in_range (150, 193) v
        | "in" -> in_range (59, 76) v
        | _ -> false
  )
  with Stdlib.Scanf.Scan_failure _ -> false

(* hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f. *)
let validate_hcl str = try 
    Scanf.sscanf str "#%[0-9a-f]" (fun v -> (String.length v) = 6)
  with Stdlib.Scanf.Scan_failure _ -> false

(* ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth. *)
let validate_ecl = function
        | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" -> true
        | _ -> false

(* pid (Passport ID) - a nine-digit number, including leading zeroes. *)
let validate_pid str = try 
    Scanf.sscanf str "%[0-9]" (fun v -> (String.length v)=9) 
  with Stdlib.Scanf.Scan_failure _ -> false

(* cid (Country ID) - ignored, missing or not. *)