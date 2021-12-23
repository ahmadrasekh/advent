open Sexplib.Std

type point = int * int [@@deriving sexp_of]
type vector = int list [@@deriving sexp_of]
type matrix = vector list [@@deriving sexp_of]
type segment = point * point [@@deriving sexp_of]

(* let transpose m =  *)

let mkSegmentV ((x1, y1), (x2, y2)) =
  let rec aux acc ((x, y) as curr) target = 
    if curr = target then (target::acc) else aux (curr::acc) (x, y+1) target in
  assert (x1 = x2);
  if y2>y1 then let tmp = aux [] (x1, y1) (x2, y2) in List.rev tmp else aux [] (x2, y2) (x1, y1)

let mkSegmentH ((x1, y1), (x2, y2)) =
  let rec aux acc ((x, y) as curr) target = 
    if curr = target then (target::acc) else aux (curr::acc) (x+1, y) target in
  assert (y1 = y2);
  if x2>x1 then let tmp = aux [] (x1, y1) (x2, y2) in List.rev tmp else aux [] (x2, y2) (x1, y1)

let mkSegmentD ((x1, y1), (x2, y2)) =
  let fx = if x1 < x2 then fun t -> t+1 else fun t -> t-1 in
  let fy = if y1 < y2 then fun t -> t+1 else fun t -> t-1 in
  let rec aux acc ((x, y) as curr) target = 
    let next = (fx x, fy y) in
    if curr = target then (target::acc) else aux (curr::acc) next target in
  aux [] (x1, y1) (x2, y2)

