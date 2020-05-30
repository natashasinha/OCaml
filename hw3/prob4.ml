(* Problem 4 in Homework 3, CSci 2041, Spring 2020 *)

(* The definitions of accumulate and reduce *)

let rec accumulate f lst u =
   match lst with
   | [] -> u
   | (h::t) -> accumulate f t (f h u)


let rec reduce f lst u =
   match lst with
   | [] -> u
   | (h::t) -> f h (reduce f t u)   


(* Solution to Part 1 *)

let append l1 l2 = reduce (fun x y -> x::y) l1 l2
                 
(* append takes in two arguments, both being lists. the goal of this function is to append list 1 to list 2. it does this by calling 
the reduce function, assigning list 1 to the variable x, list 2 to the variable y, and appending the two lists. 
the final product is one long lists that contains both the elements from x (list 1) and y (list 2).*)

(* Solution to Part 2 *)

let reverse l1 = accumulate (fun x y -> x::y) l1 []
               
(* reverse takes in one argument - a list. it then uses the accumulate function to parse through the list and append each element to the 
empty list. since one element is appended to the list at a time, the result is a list that is the reverse of the input list*)                 

(* Solution to Part 3 *)

let filter f l1 = reduce (fun x y -> if f x true then x::y else y) l1 []

 (*filter takes in two arguments - a function and a list. if the function applied to x is true, the element is appended to the list. 
 if the function applied to x is not true, then the list with any previous elements already appended, is returned and 
 no action is performed on the element at hand. *)
 
 
 
