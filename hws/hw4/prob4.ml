(* Problem 4 in Homework 4, CSci 2041, Spring 2020 *)

(* Definitions of statement forms seen in class and in the lab *)
let seq stat1 stat2 =
        fun s -> (stat2 (stat1 s))

let ifstat exp stat1 stat2 =
    fun s -> if (exp s) then (stat1 s)
             else (stat2 s)

let rec whilestat exp stat =
   fun s ->
      ifstat exp
             (seq stat (whilestat exp stat))
             (fun x -> x) s
(*Part 1*)

(* Part 1a *)

(* dostat: (state -> bool) -> (state -> state) -> state -> state *)

(* Part 1b *)

(* This function allows us to model the do-while statement. The format is 
similar to that of the whilestat function, however the final function call with 
s is different, as this represents a do-while loop as opposed to a while loop. *)
let rec dostat exp stat =
    fun s ->
      ifstat exp
             (seq stat (dostat exp stat))
             (fun s -> s) s
(* Part 2 *)

(* Part 2a *)

(* The state definition includes 3 ints as there are 3 integer variables in
 this function (i, sum, n), therefore each int is associated with one 
integer variable. *)
    
(* type state = int * int * int*)
                                  
(* Part 2b *)

(* Getter and putter functions for each of the defined variables in the function. *)
let geti (i,s,n) = i
let getsum (i,s,n) = s
let getn (i,s,n) = n

let puti exp x =
  let (i,s,n) = x in (exp x,s,n)
                   
let putsum exp x =
  let (i,s,n) = x in (i,exp x,n)
                   
let putn exp x =
  let (i,s,n) = x in (i,s,exp x)
             
(* Part 2c *)
                   
(* This function is modeled off of the factexp function on slide 47 from the lecture
slideshow "Functions as First-Class Objects". The identifier sumup is bound to an 
OCaml expression that represents
i = 0;
sum = 0;
do {i = i + 1; sum = sum + i;}
while (i < n);   *)

let zero = fun s -> 0

let i_lesseq_n = (fun x -> (geti x) <= (getn x))
let i_plus_one = (fun x -> (geti x) + 1)
let sum_plus_i = (fun x -> (getsum x) + (geti x))


let sumup =
  seq (puti zero)
    (seq (putsum zero)
      (dostat i_lesseq_n
        (seq(putsum sum_plus_i)
          (puti i_plus_one))))


(* Part 3 *)

(* The function sumToN uses the sumup function from Part 2c to return the value
of the final sum that is computed. getsum takes a positive number and sums up 
all of the numbers up to that number. 
example: getsum 7 would compute 1 + 2 + 3 + 4 + 5 + 6 + 7 and therefore return 28. *)
  
let sumToN n = getsum (sumup(0,0,n))


