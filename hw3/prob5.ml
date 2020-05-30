(* Problem 5 in Homework 3, CSci 2041, Spring 2020 *)

(* The type for expressions and the new name function given in the homework appear below *)

type expr =
       Id of string                   (* for identifiers *)
     | Int of int                     (* for integers *)
     | True                           (* for the boolean value true *)
     | False                          (* for the boolean value false *)
     | Plus of (expr * expr)          (* for exp1 + exp2 *)
     | Minus of (expr * expr)         (* for exp1 - exp2 *)
     | Times of (expr * expr)         (* for exp1 * exp2 *)
     | Div of (expr * expr)           (* for exp1 / exp2, division being for integers *)
     | Lss of (expr * expr)           (* for exp1 < exp2 *)
     | Eq of (expr * expr)            (* for exp1 = exp2, = being equality comparison *)
     | Gtr of (expr * expr)           (* for exp1 > exp2 *)
     | And of (expr * expr)           (* for exp1 && exp2 *)
     | Or of (expr * expr)            (* for exp1 || exp2 *)
     | Not of expr                    (* for not exp *)
     | Cond of (expr * expr * expr)   (* for if exp1 then exp2 else exp3 *)
     | Let of (string * expr * expr)  (* for let <id> = exp1 in exp2 *)
     | Fun of (string * expr)         (* for fun (x:ty) -> exp *)
     | App of (expr * expr)           (* for (exp1 exp2) *)

(*this is for  generating the new variable name - substitution and name change case*)
            
let namecounter = ref 0
let newname () =
     ( namecounter := !namecounter + 1; "var" ^ string_of_int !namecounter)



(* Solution to Part 1 *)

(*freeIn takes two arguments, the expression and the id name (referred to as n). it checks the first expression 
and the second expression to see if it contains the string id entered. returns true if the string name is found 
and false if not found. this function is later used in the subst function. *)
   
let rec freeIn expr n =
  match expr with
  | Id s -> if (s = n) then true else false
  | (Int _ | True | False) -> false
  | Plus(e1,e2) -> freeIn e1 n || freeIn e2 n
  | Minus(e1,e2) -> freeIn e1 n || freeIn e2 n
  | Times(e1, e2) -> freeIn e1 n || freeIn e2 n
  | Div(e1,e2) -> freeIn e1 n || freeIn e2 n
  | Lss(e1,e2) -> freeIn e1 n || freeIn e2 n
  | Eq(e1,e2) -> freeIn e1 n || freeIn e2 n
  | Gtr(e1,e2) -> freeIn e1 n || freeIn e2 n
  | Cond(e1,e2,e3) -> freeIn e1 n || freeIn e2 n || freeIn e3 n
  | And(e1,e2) -> freeIn e1 n || freeIn e2 n
  | Or(e1,e2) -> freeIn e1 n || freeIn e2 n
  | Not e1 -> freeIn e1 n
  | Let(s, e1,e2) -> if (freeIn e1 s)= true then true else false 
  | Fun(s,e1) -> if s = n then false else freeIn e1 n
  | App(e1,e2) -> freeIn e1 n || freeIn e2 n


(* Solution to Part 2 *)

(* subst takes an expression, the name of an id, and an expression to replace it with, and returns an expression that results 
from replacing all of the occurrances of the id in the first expression by the second one, if the ids match. exceptions arise 
for the let and fun pattern match cases. if the name of the id being substituted for is the same as the id bound by the let or 
function expression, then no substitution is made. *)
                
let rec subst exp1 s2 exp2 =
  match exp1 with
  | Id s -> if s = s2 then exp2 else exp1
  | Int _ -> exp1
  | True -> exp1
  | False -> exp1 
  | Plus(e1,e2) -> Plus (subst e1 s2 exp2, subst e2 s2 exp2)
  | Minus(e1,e2) -> Minus (subst e1 s2 exp2, subst e2 s2 exp2)
  | Times(e1,e2) -> Times (subst e1 s2 exp2, subst e2 s2 exp2)
  | Div(e1,e2) -> Div (subst e1 s2 exp2, subst e2 s2 exp2)
  | Lss(e1,e2) -> Lss (subst e1 s2 exp2, subst e2 s2 exp2)
  | Eq(e1,e2) -> Eq (subst e1 s2 exp2, subst e2 s2 exp2)
  | Gtr(e1,e2) -> Gtr (subst e1 s2 exp2, subst e2 s2 exp2)
  | And(e1,e2) -> And (subst e1 s2 exp2, subst e2 s2 exp2)
  | Or(e1,e2) -> Or (subst e1 s2 exp2, subst e2 s2 exp2)                      
  | App (e1,e2)  ->  App (subst e1 s2 exp2, subst e2 s2 exp2)         
  | Not (e1) -> Not(subst e1 s2 exp2)
  | Cond(e1,e2,e3) -> Cond (subst e1 s2 exp2, subst e2 s2 exp2, subst e3 s2 exp2)
  | (Let(s,e1,e2)) -> let new_e1 = subst e1 s2 exp2 in
                      if s = s2 then Let (s,new_e1,e2)
                      else if freeIn exp2 s then
                        let new_name = newname () in
                        let new_exp1 = Let (new_name, new_e1, subst e2 s (Id new_name)) in subst new_exp1 s2 exp2
                      else Let(s,new_e1, subst e2 s exp2)
                             
  | Fun(s,e1) ->  let new_e1 = subst e1 s2 exp2 in
                   if s = s2 then Fun(s,e1)
                   else
                     if freeIn exp2 s then
                       let new_name = newname () in
                       let new_exp1 = Fun(new_name,new_e1) in
                       subst new_exp1 s2 exp2
                     else Fun (s,new_e1)
              

                    
                             
