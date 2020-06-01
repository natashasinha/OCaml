(* Problem 2 in Homework 4, CSci 2041, Spring 2020 *)

(* part 1 *)
(* This is a type constructor that contains information for an olist. 
It includes the data as well as an intended ordering relation that can
 be applied to the data to determine if it is ordered or not. *)

type 'a olist = {data : 'a list; order : 'a -> 'a -> bool;}
                  
(* part 2 *)
(* This function takes an ordering relation over a given type and
 returns an empty olist of the same type specified. *)
              
let initOList order = {data = []; order = order;}
      
(* part 3 *)

(* binding list1 to a value of type int olist that uses the < ordering relation 
and is satisfied by the data provided. *)
let list1 = {data = [1;5;7;12;13]; order = fun x y -> x < y;}

(* binding list2 to a value of type int olist that uses the > ordering relation
 and is satisfied by the data provided. *)
let list2 = {data = [17;14;9;6;2]; order = fun x y -> x > y;}

(* binding list3 to a value of type string olist with the order being a lexicographic
 ordering relation and is satisfied by the data provided. *)          
let list3 = {data = ["a","b","c","d","e"]; order = fun x y -> x < y;}

(* binding list4 to a value of type int olist and an order that is not satisfied
 by the data provided. *)
let  list4 = {data = [17;14;9;13;2]; order = fun x y -> x < y;}

(* part 4 *)
(* The function isOrderedList takes an ordered list and determines if the 
associated ordering invariant is satisfied. *)
           
 let rec isOrderedList {data = l; order = order;} =
      match l with
      | [] -> true
      | _::[] -> true
      | h1::h2::t -> (order h1 h2) && isOrderedList {data = h2::t; order = order}

 (* part 5 *)
 (* The function insertOList takes in an element and orderedlist and returns an 
ordered list that has all of the original elements along with the new one. This new 
list also satisfies the ordering invariant. The helper function insert_helper takes the 
element and the previous list, and if the list is not empty the insert function then 
inserts it into the accumulated list. *)
                   
let rec insert_helper f l1 a =
  match l1 with
  | [] -> []
  | h::t -> if (f a h) then a::h::t else h::insert_helper f t a

let insertOList a {data = l; order = order} =
  match  l with
  | [] -> {data = [a]; order = order}
  | _ -> {data = (insert_helper order l a); order = order}

(* part 6 *)
(* The function olistToList takes in the ordered list and converts 
it to a "normal" list. The elements and order is not changed. *)
       
let rec olistToList {data = l; order = order} =
  match l with
  | [] -> []
  | _ -> l

