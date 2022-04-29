#use "../lib/utils.ml"

let parse_input = read_file_f explode

let adj_index_list (i,j) =
  (i-1, j)::(i+1,j)::(i,j-1)::(i,j+1)::(i-1,j-1)::(i-1,j+1)::(i+1,j-1)::(i+1,j+1)::[]

let empty = function 
  | 'L' -> true
  | _ -> false

let iter_matrix_i f m = 
  List.iteri (
    fun j is -> 
      List.iteri (fun i x -> f (i, j) x ) is
  ) m

let occupy h seat =
  let neighbours = adj_index_list seat in
  List.iter (fun s ->
    if Hashtbl.mem h seat then 
      let n = Hashtbl.find h s in
      Hashtbl.replace h s (n+1)
  ) neighbours

let leave h seat =
  let neighbours = adj_index_list seat in
  List.iter (fun s ->
    if Hashtbl.mem h seat then 
      let n = Hashtbl.find h s in
      Hashtbl.replace h s (n-1)
  ) neighbours

let init m = 
  let (x, y) = (List.length (List.hd m), List.length m) in
  let h = Hashtbl.create (x*y) in
  iter_matrix_i  (fun (i,j) s -> if (empty s) then Hashtbl.add h (i, j) 0) m;
  h

(* let traverse input adjs =
  let aux (i,j) = 
    (* if seat (not floor) *)
    if Hashtbl.mem adjs (i,j) then
      (* neighbours of seat *)
      let 
      let num_of_occupied_neighbours = Hashtbl.find adjs (i,j) in
      (* if seat can be occupied *)
      if num_of_ooccupied_neighbours < 4 then
        let neighbours = adj_index_list in
          List.iter(
            fun (i', j') ->
            if Hashtbl.mem adjs (i',j') then

          )
        (* change seet   *)
        Hashtbl.replace (i,j) (n+1) *)

let _ =
  "input" 
  |> parse_input
  |> init
  |> (fun h -> Hashtbl.length h)