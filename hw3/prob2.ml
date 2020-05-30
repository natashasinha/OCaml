(* Problem 2 in Homework 3, CSci 2041, Spring 2020 *)


(*type declaration for the elements of the binary tree*)

type 'a btree = Empty | Node of 'a * 'a btree * 'a btree

(* treemap takes in two arguments - a binary tree and a function. the function acts on the elements of the binary tree 
as input and transforms the tree into a new tree with the function applied to every element of the tree. the first pattern 
match statement checks to see if the element (at hand) of the tree is empty. if so, it returns an empty element. if not, it applies the 
function to the node at hand, as well as to the left and right subtree.*)
                              
let rec treemap btree f =
  match btree with
  | Empty -> Empty
  | Node(a,b,c) -> Node(f a, treemap b f,treemap c f) 
  
  
  
