#use "../lib/utils.ml"

let count colour bags =
  List.fold_left (fun acc (parent, children) ->
    let children = List.map snd children in 
    if (List.mem colour children) then parent::acc else acc
    ) [] bags

let count_all colour bags =
  let rec aux acc to_count =
    let tmp = to_count |> 
              List.map (fun c -> count c bags) |>
              List.flatten in
    let new_acc = List.flatten [acc; tmp] |> List.sort_uniq compare in
    if (List.length new_acc) = (List.length acc) then 
      acc 
    else aux new_acc new_acc in
  aux (count colour bags) (count colour bags)


let quantify s = (Scanf.sscanf s "%i %[a-zA-Z0-9' ']") (fun x s -> (x,s))


let parse_input =
  read_file >>
  List.map (remove ["bags"; "bag"; "\\."]) >>
  List.map (split_str_at_delims ["contains"; "contain"])
   >>
  List.map (fun x ->  (
                        List.hd x,
                        (List.tl x) |>
                        List.map (split_str_at_delims [","]) |>
                        List.flatten
                      ))


let solve (colour:string) db = 
  let rec aux (c:string) = 
      let to_solve : ((int * string)list)= List.assoc c db in 
        match to_solve with
          | [] -> 0
          | _  ->  List.fold_left(fun acc (x, y) -> 
                      acc + x + ((aux y) * x)
                    ) 0 to_solve in
  aux colour

let _ = 
  "input"
  |> parse_input 
  |> List.map (fun (x, ls) -> (x, try List.map quantify ls with _ -> []))
  (* |> count_all "shiny gold" *)
  (* |> List.length *)
  |> solve "shiny gold"