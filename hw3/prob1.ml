(* Problem 1 in Homework 3, CSci 2041, Spring 2020 *)

(* divide_list takes a boolean function over a given type, along with a list of elements of that type. 
it then divides the input list into two lists - one that contains the elements that satisfy the function, 
and another containing the elements that don't satisfy the function. *)

let rec divide_list func lst =
  match lst with
  | [] -> ([],[])
  | (h::t) -> if func h = true then let (l1,l2) = divide_list func t
                                    in (h::l1,l2)
              else let (l1,l2) = divide_list func t in (l1,h::l2)
              
