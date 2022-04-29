#use "../lib/utils.ml"

(* TODO: parse "1-3 a: abcde" to (1,3), 'a', "abcde" *)
let parse_line str = 
  let f min max ch str = ((min, max), ch, str) in
  Scanf.sscanf str "%i-%i %c: %s" f

let parse_input = read_file >> List.map parse_line

let validate_a (range, ch, str) =  in_range range (count_in_string ch str)

let validate_b ((i, j), ch, str) = 
  let char_sq = str |> String.to_seq |> List.of_seq in
  bool_of_int (
    (lxor)
    (int_of_bool ((List.nth char_sq (i-1)) = ch))
    (int_of_bool ((List.nth char_sq (j-1)) = ch))
  )

let _ = 
  "input" |>
  parse_input |>
  (List.filter validate_b) |>
  List.length
