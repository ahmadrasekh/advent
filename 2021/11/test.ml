
open Core_kernel.Std

let append stack char =
  match char, stack with
  | ')', '('::xs -> xs
  | '}', '{'::xs -> xs
  | ']', '['::xs -> xs
  | '>', '<'::xs -> xs
  | _, _ -> char :: stack

let test ls = try 
    let hd = List.hd ls in 
    match hd with
    | '}' | ')' | ']' | '>' -> Some hd
    | _ -> None 
  with _ -> None

let are_balanced str =
  Core.String.fold str ~init:[] ~f:(
    fun accum char ->
      match char with
      | '{' | '}' | '(' | ')' | '[' | ']' | '<' | '>' -> append accum char
      | _ -> accum
  ) |> test
(* |> Core.List.is_empty *)