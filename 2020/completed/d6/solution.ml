#use "../lib/utils.ml"

module ChartSet = Set.Make( 
  struct
    let compare = Pervasives.compare
    type t = char
  end )

let parse_input =
  read_file >>
  group ""

let mk_group_answer_sets = List.map (
  explode >>
  ChartSet.of_list
)

let _ =
  "input" |>
  parse_input |>
  List.map mk_group_answer_sets |>
  List.map (fun sets -> 
    List.fold_left 
      (* (fun s union -> ChartSet.union s union)  *)
      (fun s intersect -> ChartSet.inter s intersect) 
      (List.hd sets) 
      (List.tl sets)
  ) |>
  List.map ChartSet.cardinal |>
  list_add
