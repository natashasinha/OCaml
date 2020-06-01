(* Copyright (c) Gopalan Nadathur *)

(* Lab 12, CSci 2041, Spring 2020 *)

(* Problem 2, part 1 *)

module type ITEM =
 (* the last component, initial, is meant to identify a value of item type
    that can be used in initialization *)
    sig
        type item
        val leq : item * item -> bool
        val initial : item
    end

(* Provide definitions of IntItem and StringItem here *)
module IntItem : (ITEM with type item = int)=
  struct
    type item = int
    let leq((p:item),(q:item)) = p<= q
    let initial = 0
                
  end

module StringItem : (ITEM with type item = string) =
  struct
    type item = string
    let leq((p:item),(q:item)) = p<= q
    let initial = " "
                
  end
  
(* For Problem 2, part 2 *)

module type HEAP =
  sig
    type item
    type tree
    exception InitHeap
    val depth : tree -> int
    val initHeap : int -> tree
    val insert : item * tree -> tree
    val isHeap : tree -> bool
    val maxHeap : tree -> item
    val replace : item * tree -> item * tree
    val size : tree -> int
    val top : tree -> item
  end


(* Using the code in heapcode.ml, define a functor called Heap that
takes a module satisfying the ITEM signature and produces a module satisfying
the HEAP signature here. *)

module Heap (Item : ITEM) : (HEAP with type item = Item.item) =
  struct
    type item = Item.item

        let leq(p, q) : bool = Item.leq(p,q)

        let max(p,q) = if leq(p,q) then q else p
        and min(p,q) = if leq(p,q) then p else q

        let intmax((p : int),q) = if p <= q then q else p

        type tree =
          | L of item
          | N of item * tree * tree

       exception InitHeap

       let rec initHeap n =
           if (n < 1) then raise InitHeap
           else if n = 1 then L Item.initial
                else let t = initHeap(n - 1)
                     in N (Item.initial, t, t)

        let rec top t =
          match t with
          | (L i) -> i
          | N (i,_,_) -> i


        let rec isHeap t =
          match t with
          | (L _) -> true
          | (N(i,l,r)) ->
            leq(i,top l) && leq(i,top r) && isHeap l && isHeap r

        let rec depth t =
          match t with
          | (L _) -> 1
          | N(i,l,r) -> 1 + intmax(depth l,depth r)

       let rec replace (i,h) = (top h, insert(i,h))
       and insert (i, h) =
         match h with
         | L _ -> L i
         | N (_,l,r) ->
           if leq(i,min(top l,top r))
           then N(i,l,r)
           else if leq((top l),(top r))
                then N(top l,insert(i,l),r)
                else N(top r,l,insert(i,r))

       let rec size h =
         match h with
         | L _ -> 1
         | N (_,l,r) -> 1 + size l + size r

       let rec maxHeap h =
         match h with
         | (L i) -> i
         | N (_,l,r) -> max(maxHeap l, maxHeap r)

  end    (* end of struct *)
  
(* Problem 2, part 3 *)

(* Uncomment the lines below to get integer and string heaps. *)

module IntHeap = Heap(IntItem)
module StringHeap = Heap(StringItem)

(* Problem 3, Code to experiment with and then comment on *)

let cond (c,t,e) =
   match c with
   | true -> t
   |  false -> e

let rec fact n =
   cond (n=0,1, n * fact (n-1))

(* Write your explanation for why this code does not work here:
    This will not work since OCaml does call by value, it will first evaluate the e argument all the
    way before the other arguments. When it gets to fact 0, it will go to fact -1. It will then be infinitely 
    recursive since there is nothing in place to stop the recursion as it is past 0.  
 *)

(* Problem 4 *)

(* The code that you have to consider in this problem *)

  let rec append l1 l2 =
     match l1 with
     | [] -> l2
     | (h::t) -> h::(append t l2)

  let head l =
     match l with
     | [] -> 0
     | (h::t) -> h

(* The expression whose evaluation you have to consider:
       head (append (append [1;2] [3]) [4])
Part 1: Show the steps in call-by-name evaluation below
Step 1: 3 will be appended to the list containing 1 and 2 
new list: [1;2;3]
Step 2: 4 will be appended to the list containing 1, 2 and 3
new list: [1;2;3;4]
Step 3: the head of the most recently modified list is taken
output: 1
Part 2: Show the steps in call-by-value evaluation below
Step 1: the head of the list is taken
output: 1
Step 2: the head is then appended to the 3 using the innermost append function
new list: [1;3]
Step 3: 4 is appended to the list from the innermost append function
new list: [1;3;4]
*)
