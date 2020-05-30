(* Solution to Problem 1 *)

(* 1. Well typed; int = -4 *)
(* 2. Not well typed, it is not using the +. operator (float type) *)
(* 3. Not well typed; the 7 is not in float form *)
(* 4. Well typed; int = 7 *)
(* 5. Not well typed; else statement contains a string/non-int value *)
(* 6. Not well typed; there is no else type*)
(* 7. Well typed; value = "hello world"*)
(* 8. Not well typed; it is trying to concatenate a function and a string*)

(* Solution to Problem 2 *)

(* 1. Not legal; y is not defined *)
(* 2. Legal; int = 3 *)
(* 3. Not legal; anything before the "and" does not carry over, they would
have to be 2 separate expressions*)
(* 4. Legal; int = 3 *)
(* 5. Legal; int = 5*)

(*Solution to Problem 3 *)

(*This function takes in two integer arguments a and b*)
(*If one number is 0, the other number is returned*)
(*It calculates the gcd by taking the first number mod the second,
and recursively calls the gcd function with the first number and 
the result of a mod b.*)

let rec gcd a b =
        if b = 0 then a else gcd b (a mod b);;


(*Solution to Problem 4*)

(*This function takes in two integer arguments*)
(*If the gcd of the two numbers is 1, the numbers are returned.
This means that the rational number is already in the reduced form*)
(*Otherwise it finds the gcd of the two numbers, divides both numbers
by the gcd, and recursively does this until the rational number is reduced 
and the gcd is 1.*)

let rec reduced_form (a,b) = 
         if (gcd a b) = 1 then (a,b)
         else  (a / (gcd a b), b / (gcd a b));;
         
         
(*Solution to Problem 5*)

(*This function takes in two numbers*)
(*If the first number is smaller than the second, then an empty list
is returne.d*)
(*Otherwise a list of numbers from the first to the second is printed out 
(descending order.*)

let rec fromMtoN m n =
         if n>m then []
         else if m=n then [m]
         else
            m:: (fromMtoN(m-1)n);;

      
(*Solution to Problem 6*)

(*This function takes in a list of non-type specific elements*)
(*It then finds the elements at the odd index positions and returns them
in a list by finding the first odd index and recursively finding every
one after.*)

let rec everyOdd lst = 
        match lst with
        | [] -> []
        | [h] -> [h]
        | (h::m::t) -> h::(everyOdd t)

            ;;
 

(* Solution to Problem 7*)
        
(*This function takes in 2 arguments - a list of numbers and a positive
integer i*)
(* It then uses a recursive helper function to find all of the elements
located at indexes which are multiples of the integer i entered, and returns
them in a list.*)
        
        let rec everyNth lst i =
          let rec helper lst idx =
            match lst with
            | [] -> []
            | h::t ->
               if i = idx then h :: (helper t 1)
               else helper t(idx+1)
            in helper lst 1


(*Solution to Problem 8*)
             
(* This function takes in a list of tuples containing names, phone numbers, 
and salary amounts for employees, along with a name entered as a string*)
(*It then list matches the entered string name with the names in
the list/database and returns the corresponding salary.*)
             
        let rec find_salary lst name =
          match lst with
            | [] -> 0.0
            | (h,m,t)::tail ->
                  if h = name then t
                  else find_salary tail name
                             
(* This function is similar to the one above, but instead of printing
out the salary that goes along with the entered name, it prints out 
the corresponding phone number.*)
                
        let rec find_phno lst name =
          match lst  with
          | [] -> "No number found"
          | (h,m,t)::tail ->
             if h = name then m
             else find_phno tail name
            
                    
(*Solution to Problem 9*)

(* 1. Each list has to have the same number of elements *)

(* 2. *)
(*This function takes in a matrix through the format of a list of lists. It then checks to see 
if each row in the matrix is equal in length (if each list is the same length) through the use of a recursive 
helper function and then returns true if so, otherwise returns false.*)

       let length list =
         let rec aux n = function
            |[] -> n
            |_:: t -> aux(n+1) t
            in aux 0 list

        let rec is_matrix lst =
          match lst with
          | [] -> false
          | _::[] -> true
          | h::m::t ->
             if (length h) = (length m) then true
             else false

(* 3. *)
(*This function takes in a list of integer lists, along with another integer to act as the scale. 
It then uses a helper function to recursively multiply each integer element in the list by the input 
scale number. It does this for every list entered and then returns a new list with the scaled elements 
and lists.*)


   let rec multiply_list lst num =
      match lst with
      | [] -> []
      | h::t -> ((num*h)::(multiply_list t num))
            
   let rec matrix_scalar_multiply lst num =
      match lst with
      | [] -> []
      | _::[] -> []
      | h::t -> (multiply_list h num)::(matrix_scalar_multiply t num)
