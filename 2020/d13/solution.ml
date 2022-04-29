let earliest arrival bus = bus - (arrival mod bus)

let solve = earliest 1001171

let bues = [17;41;37;367;19;23;29;613;13]

let solve = 
    List.map (fun x -> (x, solve x)) bues |>
    List.sort (fun (_, x) (_, y) -> compare x y) 