  (* Copyright (c) Gopalan Nadathur *)


(* Problem 1 *)

(* The code given to you at the outset *)

type 'a stream = Stream of (unit -> 'a * 'a stream)

let mkStream f = Stream f
let nextStream (Stream f) = f ()

let rec fromNStream n = mkStream (fun () -> (n, fromNStream (n+1)))
let natStream = (fromNStream 0)

(* Write the definition of zipStreams below *)

let rec zip s1 s2 () =
  let (x,rst) = nextStream s1 in
  (x, Stream(zip s2 rst))

let zipStreams s1 s2 = Stream (zip s1 s2)
                     
(* source: ocaml.org/learn/tutorials/streams.html *)
              
(* Problem 2 *)

(* The code given to you to analyze *)
let rec fib_aux n i f s =
    if (n = i) then f
    else fib_aux n (i+1) s (f+s)
    
let fib n = fib_aux n 1 1 1

(* Part 1
   The recurrence equation for the running time for
   (fib_aux n i f s) expressed as a function of (n-i).
            T(n) = T(n-1) + T(n-2) + c2
*)

(* Part 2
   Replace these sentences with your guess of a solution for the recurrence
   relation. Then include an inductive proof of the fact that your guess is
   correct.
   If you are unable to provide an inductive proof, then at least include
   a rationale underlying your guess.
*)

(* Part 3
   The time complexity of this version of fib will be less than the naive function, but will be
   more costly than the memoized version. The naive version will go back and repeat computations 
   that have already been performed, therefore being more costly. The memoized version will be more 
   efficient because less time and space will be spent on performing computations that have already been done, 
   since the computations will be remembered.
*)
   

(* Problem 3 *)

(* The type declarations that set the problem up *)

type color = R | B

type 'a rbtree =
    Empty
  | Node of color * 'a * 'a rbtree * 'a rbtree

(* Part 1 *)

(*  is_RBTree_aux : 'a rbtree -> int * bool   *)

let is_RBTree_aux t =
  match t with
  | Empty | Node(x,l,r) -> (n, true)
  | _ -> (n, false)
  

(* Part 2 *)

(* is_RBTree : 'a rbtree -> bool *)

let is_RBTree t =
  match (is_RBTree_aux t) with
  | (n,true) -> true
  | (n,false) -> false 

(* Part 3 *)

(*  bh_RBTree : 'a rbtree -> int option *)

let bh_RBTree t =
  match t with
  | n -> Some n
  | _ -> None
  
  
  
