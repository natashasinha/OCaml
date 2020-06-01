(* Problem 3 in Homework 3, CSci 2041, Spring 2020 *)

(* Solution to Part 1 *)

let rec reduce f lst u =
   match lst with
   | [] -> u
   | (h::t) -> f h (reduce f t u)   


(*
1. We assume that reduce is of type 'a 
2. Based on the function declaration we can see that reduce is a function, so the type is refined to 'a -> 'b where 'a is the 
list type and 'b is the type of the output
3. The second line does not reveal any new information
4. The third line shows that it is being pattern matched against the empty list [], so lst changes from 'a to 'a list
5. From this pattern, we can see that the output of reduce is an element, so the type of reduce gets refined to 'a list -> 'b
6. In the fourth line we can see that lst is consistent with the previous idea that lst is of type 'a list, therefore not 
causing any conflicts. h is assigned 'a and t is assigned type 'a list
7. The fourth line also contains h with function f applied to it, along with a recursive call to reduce with the function 
and the tail (new list) of the initial list.This shows that h is refined to be of type 'b. The recursive call to reduce contains 
the arguments f (function) along with the tail and the element u. When this is recursively called, it will either output u which is 
of type 'b, or go back to the second pattern match case which is of type 'b and a recursive call.
8. We can now see that both of the pattern match cases will lead to outputs of the same type, therefore allowing us to conclude 
that the type of reduce is 'a -> 'b *)
             

(* Solution to Part 2 *)

let rec forall2 p l1 l2 =
   match (l1,l2) with
   | ([],[]) -> true
   | ([],_) -> false
   | (_,[]) -> false
   | ((h1::t1),(h2::t2)) ->
         (p h1 h2) && (forall2 p t1 t2)
         

(*
1. We first assume that forall2 is of type 'a
2. Based on the first line, we see that forall2 is a function, so the type gets refined to ('a,'b) -> ('c,'d) where ('a,'b) is the 
type of input tuple and ('c,'d) is the type of the output
3. The second line does not provide any relevant information
4. The third line shows that the tuple (l1,l2) is being pattern matched against a tuple containing empty lists ([],[]), so the type 
is refined to ('c,'d) -> ('a list,'b list)
5. Based on the result of this pattern, we can see that the output of this match statement must be a boolean value, so the type of 
forall2 is refined to type bool
6. The pattern match statement in lines 4 and 5 also allow us to see that the putput is of type bool
7. The fourth pattern match statement is consistant with the idea that the we are dealing with a tuple containing 2 lists, 
therefore not presenting any conflicts. It also assigns 'a to the headers of each list and 'a list to the tails
8. Based on the result of this pattern match we see that the function p with arguments h2 and h2 must of type boolean along with the 
recursive call back to the forall2 function due to the && operator. We also need to make sure that the inputs for the recursive 
call type check out. In the recursive call, forall2 is calling p with the tail of the first list in the tuple and the tail of the 
second list in the tuple. Since the recursive call is being called along with the function p, the result will be of type bool as well. 
9. Everything is type correct, and the final type of forall2 can be summarized as ('a -> 'b -> bool) -> 'a list -> 'b list -> bool, 
where the final types of the match statements all eventually return a boolean type.
 *)
