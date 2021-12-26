open Utils.Util
open Sexplib.Std
open Utils.LinearAlgebra

(** 
   find number of distict paths such that on any path
   small caves are visited at most once
*)

type t = string list [@@deriving sexp_of]

let parse_line = fun x -> x

let parse_input () = "./input" |>
                     read_file |>
                     parse_line

let () = 
  parse_input () |>
  sexp_of_t |>
  Core.Sexp.to_string |>
  print_endline
