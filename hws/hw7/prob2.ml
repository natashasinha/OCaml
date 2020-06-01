 (* Solution to Problem 2, Homework 7, CSci 2041, Spring 2020 *)

(* First, some declarations providing the stream abstraction *)

type 'a stream =
   Stream of (unit -> 'a * 'a stream)

let mkStream f = Stream f
let nextStream (Stream f) = f ()

(* Next some definitions that provide a natural number stream *)

let rec fromNStream n = mkStream (fun () -> (n, fromNStream (n+1)))

let natStream = (fromNStream 0)


(* Solution to Part 1, Problem 2 *)

(* The function mapStream takes a function and a stream of suitable types and produces a new 
stream that corresponds to mapping the function over the items in the input stream.               
mapStream : ('a -> 'b) -> 'a stream -> 'b stream *)

let rec mapStream f s =
  let (x,rst) = nextStream s in mkStream (fun() -> (f x, mapStream f rst))


(* Solution to Part 2, Problem 2 *)

(*  squareStream : int stream
     cubeStream : int stream   
     
squareStream - mapping using the definition of square -> x * x
cubeStream - mapping using the definition of cube -> x * x * x    *)

let squareStream = (mapStream (fun x -> x*x) natStream)
let cubeStream = (mapStream (fun x -> x*x*x) natStream)

   
(* Solution to Part 3, Problem 2 *)

(* squarecubeStream corresponds to a stream of numbers that are both the squares and the cubes of other numbers.
  squarecubeStream : int stream  
a helper function is used to find where both the cube and square function should move forward *)
               
let rec helper square cube =
  let (x1, square1) = nextStream square in
    let (x2, cube1) = nextStream cube in
       if (x1 == x2) then mkStream (fun() -> (x1, helper square1 cube1))
       else if x1 > x2 then helper square cube1
       else helper square1 cube

 let squarecubeStream = helper squareStream cubeStream
 
 
