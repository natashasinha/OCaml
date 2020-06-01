(*Solution to Problem 1*)

(* 1.Successful - A list is associated with the identifier a*)
(* 2. Unsuccessful - The expression is trying to match an int to an int list *)
(* 3. Unsuccessful - This is again trying to match an int to an int list. It
also tries to patern match with a tuple, and therefore is seen as too many
arguments by the compiler *)
(* 4. Successful - This is too specialized because when it is type checking it
is going back to the function, which we do not need. *)


(*Solution to Problem 2*)

let atLeastTwo =
    function
      | ([] | [_]) -> false
      | _ -> true

           
(*Solution to Problem 3*)

let rec sumList list count =
  match list with
      | [] -> []
      | _::[] -> []
      | (h::t) -> (sumList t, (h+count))
                              
(* The loop invariant is that the list has to either be empty, or
has to be one more than the previous count*)

(*Solution to Problem 4*)

let sumList list =
  match lst with
      | [] -> []
      | _::[] -> []
      | (h::t) ->
         let rec sumList' list count =
           (sumList' list,(h+count))

 (*Solution to Problem 5*)

 let rec drop (n,l) =
   match (n,l) with
   | () -> ()
   | (n,l) -> if n = 0 then 1 else drop ((n-1),t)

(*Solution to Problem 6*)

(*Defines the type shape and creates value constructors
Circ, Triangle, and Quadrangle for this type*)
   type shape =
     | Circ of coord * float
     | Triangle of coord * coord * coord
     | Quadrangle of coord * coord * coord * coord

   (*Helper function used to calculate the perimeter. This calls the other
helper function distance for every coordinate pair within the shape. It then
adds all of the distances together to calculate the perimeter of the entire
shape.*)
                   
   let perimeter shape =
     match shape with
     |Circ(_, radius) -> 2*. pi *.radius
     |Triangle((a1,a2),(b1,b2),(c1,c2)) -> distance((a1,a2),(b1,b2)) +
                                             distance((b1,b2),(c1,c2)) +
                                             distance((a1,a2),(c1,c2))
     |Quadrangle((a1,a2),(b1,b2),(c1,c2),(d1,d2))-> distance((a1,a2),(b1,b2)) +
                                                    distance((b1,b2),(c1,c2)) +
                                                    distance((c1,c2),(d1,d2)) +
                                                    distance((a1,a2),(d1,d2))

   (*Defines constant variable pi*)
                                                  
   let pi = 4.0 *. atan 1.0;;
   (*Helper function distance - calculates the distance between two
input coordinate points using the distance formula.*)
   
   let distance =
     function
     | ((x1,y1),(x2,y2)) ->
        let xdiff = x1 -. x2 in
        let ydiff = y1 -. y2 in
            sqrt (xdiff *. xdiff +. ydiff *. ydiff)
            
