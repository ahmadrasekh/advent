
let (>>) f g x = g(f(x))

let read_file file = Core_kernel.In_channel.read_lines file

let sum = function
  | [] -> []
  | hd::[] -> hd
  | hd::tl -> List.fold_left (fun xs ys -> List.map2 (+) xs ys) hd tl

let flip_bits = 
  String.map (function | '0' -> '1' | '1' -> '0' | _ -> failwith "non binary char")

let bin_to_dec str = int_of_string (String.concat "" ["0B"; str])

let same_bin_at index pattern line = 
  assert((String.length pattern) = (String.length line));
  (String.get pattern index) = (String.get line index)

let transpose x = 
  let rec aux acc = function 
    | []::_ -> acc
    | rows -> let tmp = (List.map List.hd rows) in 
      let rest = List.map List.tl rows in
      aux (tmp::acc) rest in
  aux [] x

let count_unique_elements list =
  let counter = Hashtbl.create 10000 in
  let update_counter x = 
    if Hashtbl.mem counter x then
      let current_count = Hashtbl.find counter x in
      Hashtbl.replace counter x (succ current_count)
    else
      Hashtbl.replace counter x 1
  in
  List.iter update_counter list;
  Hashtbl.to_seq counter
  |> List.of_seq

let memoize f =
  let h = Hashtbl.create 100 in 
  let rec fct x = match (Hashtbl.find_opt h x ) with
    | Some t -> t
    | None -> (let t = f fct x in
               Hashtbl.add h x t;
               t) in fct

let sort_str s = 
  String.to_seq s |> List.of_seq |> List.sort Char.compare |> List.to_seq |> String.of_seq;;

let cut_list_at ls delim = Core.List.fold_until
    ~init:[]
    ~f:(fun acc x -> 
        if x <> delim 
        then Base.Continue_or_stop.Continue(x::acc) 
        else  Base.Continue_or_stop.Stop acc)
    ~finish:(fun x -> x) (List.rev ls)

let matrix_safe_get m (x, y) = try Some m.(y).(x) with _ -> None

let matrix_map f = Array.map (fun r -> Array.map f r )

let matrix_find_index f m =
  Core.Array.fold_until
    ~init: (0,0)
    ~f:(fun (_, j) row -> 
        match Core.Array.findi ~f:(fun _ n -> f(n)) row with
        | Some (i, _) -> Base.Continue_or_stop.Stop (Some (i, j))
        | None -> Base.Continue_or_stop.Continue((0, j+1))
      )
    ~finish:(fun _ -> None) m

let matrix_map_update_i m (x, y) f = match matrix_safe_get m (x, y) with
  | Some a -> m.(y).(x) <- f(a)
  | None  -> () 

let matrix_find_indices f m = (Core.Array.mapi 
                                 ~f:(
                                   fun j row -> Core.Array.filter_mapi 
                                       ~f:(
                                         fun i n -> 
                                           if f(n) then Some (i, j) else None
                                       ) row
                                 ) m )|>
                              Array.map Array.to_list |>
                              Array.to_list |>
                              List.flatten