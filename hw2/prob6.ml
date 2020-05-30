(* Solution to Problem 6, Homework 2, CSci 2041, Spring 2020 *)

(* a data type for representing a simple subset of OCaml expressions *)
type expr =
       Id of string                     (* for identifiers *)
     | Int of int                       (* for integers *)
     | True                             (* for the boolean value true *)
     | False                            (* for the boolean value false *)
     | Plus of expr * expr              (* for exp1 + exp2 *)
     | Minus of expr * expr             (* for exp1 - exp2 *)
     | Times of expr * expr             (* for exp1 * exp2 *)
     | Div of expr * expr               (* for exp1 / exp2, division being for integers *)
     | Lss of expr * expr               (* for exp1 < exp2 *)
     | Eq of expr * expr                (* for exp1 = exp2, = being equality comparison *)
     | Gtr of expr * expr               (* for exp1 > exp2 *)
     | And of expr * expr               (* for exp1 && exp2 *)
     | Or of expr * expr                (* for exp1 || exp2 *)
     | Not of expr                      (* for not exp *)
     | Cond of expr * expr * expr       (* for if exp1 then exp2 else exp3 *)
     | Let of string * expr * expr      (* for let <id> = exp1 in exp2 *)


(* The examples from the problem writeup *)

let exp1 = Let ("x",(Int 5),Let ("y",(Int 7),Plus ((Id "x"),(Id "y"))))
let exp2 = Let ("x",True,Cond (Not (Id "x"),True,False))
let exp3 = Let ("x",
                False,
                Cond (Not (Id "x"),
                      Let ("x",Int 5,Plus (Id "x", Int 3)),
                      Let ("y",Int 7, Id "y")))


(*   eval : expr -> (string * expr) list -> expr  *)

let rec find lst n =
  match lst with
  | [] -> None
  | h::t -> let(x,y) = h
            in if (x = n) then Some y else find t n


let rec eval exp new_list =
  match exp with
  | (Id e1) -> Some e1
  | (Int e1) -> Some e1 
  | (True) -> Some true
  | (False) -> Some false 
  | (Plus(e1,e2)) -> match (e1,e2) with
                     | (Int x,Int y) -> x + y
                     | (Int x,Id y) -> x + (find new_list y)
                     | (Id x,Int y) -> (find new_list x) + y
                     | (Id x, Id y) -> (find new_list x) + (find new_list y)
  | (Minus(e1,e2)) -> match(e1,e2) with
                     | (Int x,Int y) -> x - y
                     | (Int x,Id y) -> x - (find new_list y)
                     | (Id x,Int y) -> (find new_list x) - y
                     | (Id x, Id y) -> (find new_list x) - (find new_list y)
  | (Times(e1,e2)) -> match (e1,e2) with
                     | (Int x,Int y) -> x * y
                     | (Int x,Id y) -> x * (find new_list y)
                     | (Id x,Int y) -> (find new_list x) * y
                     | (Id x, Id y) -> (find new_list x) * (find new_list y)
  | (Div(e1,e2)) -> match (e1,e2) with
                     | (Int x,Int y) -> x / y
                     | (Int x,Id y) -> x / (find new_list y)
                     | (Id x,Int y) -> (find new_list x) / y
                     | (Id x, Id y) -> (find new_list x) / (find new_list y)
  | (Lss(e1,e2)) -> match (e1,e2) with
                     | (Int x,Int y) -> if x < y then true else false
                     | (Int x,Id y) -> if x < (find new_list y) then true else false
                     | (Id x,Int y) -> if (find new_list x) < y then true else false
                     | (Id x, Id y) -> if (find new_list x) < (find new_list y) then true else false
  | (Eq(e1,e2)) -> match (e1,e2) with
                     | (Int x,Int y) -> if x = y then true else false
                     | (Int x,Id y) -> if x = (find new_list y) then true else false
                     | (Id x,Int y) -> if (find new_list x) = y then true else false
                     | (Id x, Id y) -> if (find new_list x) = (find new_list y) then true else false
  | (Gtr(e1,e2)) -> match (e1,e2) with
                     | (Int x,Int y) -> if x > y then true else false
                     | (Int x,Id y) -> if x > (find new_list y) then true else false
                     | (Id x,Int y) -> if (find new_list x) > y then true else false
                     | (Id x, Id y) -> if (find new_list x) > (find new_list y) then true else false
  | (And(e1,e2)) -> if (find new_list e1) && (find new_list e2) then true else false
  | (Or(e1,e2)) -> if (find new_list e1) || (find_list e2) then true else false
  | (Not(e1)) -> if (not e1) then true else false
  | (Cond(e1,e2,e3)) -> if e1 = true then eval e2 new_list
                       else eval e3 new_list
  | (Let(str,e1,e2)) -> eval e2 ((str,e1)::new_list)
