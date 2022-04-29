#use "../lib/utils.ml"

let parse_input = read_file >> List.map int_of_string

let solve ~sum_of:sm ~sum:total ~input:input_list = 
  let h = Hashtbl.create (List.length input_list) in

  let rec sum_of_two sum = function 
    | hd::tl ->  let need = sum-hd in 
      if Hashtbl.mem h (need) then
        [hd; need]
      else begin
        Hashtbl.add h hd hd;
        sum_of_two sum tl
      end
    | [] -> [] in

  let rec sum_of_three sum = function
    | hd::tl -> (
        match sum_of_two (sum-hd) tl with
        | [] -> sum_of_three sum tl
        | _ as sum_of_two_solution -> hd::sum_of_two_solution
      )
    | [] -> [] in

  match sm with
    | 2 -> sum_of_two total input_list
    | 3 -> sum_of_three total input_list
    | _ -> []

let _ = 
  "input" |> 
  parse_input |> 
  (fun ls -> solve ~sum_of:3 ~sum:2020 ~input:ls) |>
  list_multiply
