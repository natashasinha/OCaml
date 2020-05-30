(* Solution to Problem 5, Homework 2, CSci 2041, Spring 2020 *)

(* The type declarations included with the problem writeup. *)

type ty = BoolTy | IntTy | FunTy of ty * ty

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
     | Fun of string * ty * expr        (* for fun (x:ty) -> exp *)
     | App of expr * expr               (* for (exp1 exp2) *)


(* Solution to Part 1 *)

(* Complete the let bindings below as required by the problem writeup and
delete this text. *)                                    

let exp1 = Let("x",Int 5,Let("y",Int 7,Plus(Id "x", Id "y")))

let exp2 = Fun("x",(Int,(Fun("y",(Bool))),Cond(("y",True),Plus((Id "x"),(Int 1))), Minus((Id "x",(Int 5))))

let exp3 = Fun("x",Int Ty,Fun("y",Int Ty,Plus(Id "x",Id "y",(Int 5))))
    
(* Solution to Part 2 *)

(*    typeof_aux : expr -> (string * ty ) list -> ty option     *)
             
let rec typeof_aux expr lst =
  match expr with
  | (Id(e1)) -> Some IntTy
  | (True | False) -> Some BoolTy
  | (Int(e1)) -> Some IntTy
  | (Plus(e1,e2) | Minus(e1,e2) | Times(e1,e2) | Div(e1,e2) |
      Lss(e1,e2) | Gtr(e1,e2) | Eq(e1,e2) | And(e1,e2) |
      Or(e1,e2)) -> if typeof_aux e1 lst = typeof_aux e2 lst
                    then typeof_aux e1 lst
                    else None
  | (Not(e1)) -> typeof_aux e1 lst
  | (Cond(e1,e2,e3)) -> if typeof_aux e2 lst = typeof_aux e3 lst
                        then typeof_aux e2 lst
                        else None
  | (Let(s,e1,e2)) -> if typeof_aux e1 = typeof_aux e2 (s::lst)
                      then typeof_aux e2
                      else None
  | (Fun(s,e1,e2)) -> if typeof_aux s lst = Some StringTy &&
                           typeof_aux e1 lst = typeof_aux e2 lst
                      then typeof_aux e2 lst
                      else None
  | (App(e1,e2)) -> if typeof_aux e1 lst = FunTy(ty1,ty2) &&
                         typeof_aux e2 lst = ty1
                    then typeof_aux e2 lst
                    else None
                           
                        
(* Solution to Part 3 *)

(* typeof : expr -> ty option *)

let typeof expr =
  match typeof_aux expr with
  | None -> None
  | Some IntTy | Some BoolTy | Some FunTy -> Some Ty
  
  
