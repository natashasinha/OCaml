(* Solution to Problem 4, Homework 2, CSci 2041, Spring 2020 *)

(* Solution to Part 1 *)

(* A type declaration that identifies a type constructor btree that takes arguments 
corresponding to key and data values that are used to build the tree   *)

type ('a,'b) btree =
  Empty | Node of 'a * 'b * ('a,'b) btree * ('a,'b) btree

(* Solution to Part 2 *)
                
(* binds the identifier inittree to an empty tree *)
                
let inittree = Empty

(* Solution to Part 3 *)

(* tree that contains integer keys and string data *)
             
let tree1 = Node(3,"three",
                 Node(2,"two", Empty, Empty),
                 Node(5,"five", Empty, Empty))
          
(* tree that contains string names, string phone numbers, and real salaries *)    

let tree2 = Node("David",("x123",22.3),
                 Node("Britney",("x456",70.3), Empty, Empty),
                 Node("Meghan",("x134",100.2),Empty,Empty))


(* Solution to Part 4 *)

(* find takes a binary search tree as the input and returns the data
value corresponding to the key *)

(*   find : ('a, 'b) btree -> 'a -> 'b option  *)
          
let rec find tree key =
  match tree with
  | Empty -> None
  | Node(k,v,l,r) -> if k = key then Some v
                     else if (k<key) then find r key
                     else find l key
              
(* Solution to Part 5 *)

(* insert takes a binary search tree, a key, and a data item, and 
returns a new binary search tree that represents the old binary search
tree with thte new key and data pair inserted into the correct position *)
                   
(*   insert : ('a, 'b) btree -> 'a -> 'b -> ('a, 'b) btree  *)

let rec insert tree key data =
  match tree with
  | Empty -> Node(key,data,Empty,Empty)
  | Node(k,d,l,r) -> if k = key then Node(key,data,l,r)
                     else if (k<key) then Node(k,d,l,insert r key data)
                     else Node(k,d,insert l key data,r)
                       
      

(* Solution to Part 6 *)

(* keylist : ('a, 'b)btree -> ('a list)    *)

let keylist tree =
  let rec keylist_helper a lst =
    match a with
    | Empty -> []
    | Node(k,d,l,r) -> (keylist_helper l lst) @ [k] @ (keylist_helper r lst)

  in keylist_helper tree []


(* Solution to Part 7 *)

(* delete takes a binary search tree and a key and returns a new binary search 
tree that is similar to the old binary tree, but without the key and data value
associated with the entered key   *)
   
let delete tree key =
  if (find tree key == None) then tree
  else match tree with
       | Node(k,d,l,r) -> if (key == find k d) then Node(delete k d,l,r)
                          
                            
      
