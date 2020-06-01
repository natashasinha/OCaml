(* Problem 3 in Homework 4, CSci 2041, Spring 2020 *)

(* part 1 *)
(* The function cont_reverse is a tail-recursive implementation of the reverse
 function shown in the assignment. It uses continuation control-flow. *)

let rec cont_reverse l c =
  match l with
  | [] -> (c l)
  | (h::t) -> cont_reverse t (fun x -> c(x@[h]))

(* part 2 *)
(* The function cont_sumTree is a tail-recursive implementation of the sumTree 
function shown in the assignment. It uses continuation control-flow. *)
   
(* The btree type constructor as defined in the homework writeup. *)
type 'a btree = Empty | Node of 'a * 'a btree * 'a btree

let rec cont_sumTree t c =
  match t with
  | Empty -> (c 0)
  | Node(i,r,l) -> cont_sumTree r (fun y -> c (cont_sumTree l (fun x -> (x+y+i))))
