#use "../lib/utils.ml"


let parse_input f =
  let lines = read_file f in
  let visited = Array.make (List.length lines) false
  and tape = 
    lines |> 
    List.map (fun s -> Scanf.sscanf s "%3s %s" (fun cmd op -> (cmd, (int_of_string op)))) |>
    Array.of_list  in
  (visited, tape)

let run (visited, tape) =
  let rec aux i acc =
    if i=(Array.length tape) then acc
    else if visited.(i) then failwith "loop" else begin
      visited.(i) <- true; 
      let (cmd, op) = tape.(i) in
      match cmd with 
        | "nop" -> aux (i+1) acc
        | "acc" -> aux (i+1) (acc+op)
        | "jmp" -> aux (i+op) acc
        | _ -> failwith "unrecognised command!"
    end in
  aux 0 0

let collect arr =
  Array.fold_left (fun (i,acc) elem -> match elem with 
                      | ("nop",_) 
                      | ("jmp", _) -> (i+1, (i, elem)::acc)
                      | _ -> (i+1, acc)
                      ) (0, []) arr 
let _ =
  let switch (cmd, op) = match cmd with
    | "nop" -> ("jmp", op)
    | "jmp" -> ("nop", op) 
    | _ -> failwith "unrecognised command!" in
  
  let (_, tape ) = "input" |> parse_input in
  let (_, collection) = collect tape in

  let rec aux = function 
    | ((i,ins)::tl) -> (try
                          tape.(i) <- (switch ins);
                          run (Array.make (Array.length tape) false, tape)
                        with 
                          | _ -> tape.(i) <- ins; aux tl
                       )
    | [] -> failwith "empyt" in
 
  aux (List.rev collection)
