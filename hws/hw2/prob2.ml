(* Solution to Problem 2, Homework 2, CSci 2041, Spring 2020 *)

 (* Relation between the inputs and outputs *)
 (* fib(n) = fib(n-1) + fib(n-2)*)

let rec fib' fib1 fib2 i n0 =    
  if n0 = 0 then fib1 else (fib') fib2 (fib1 + fib2) (i+1) n0
(* if the input is 0 then it returns fib1, otherwise it calculates the fib number *)
  
let fib n0 =
  if n0 <= 2 then 1 else fib' 1 1 1 n0 
 (* main function calling the aux function with the values set at initial *)
