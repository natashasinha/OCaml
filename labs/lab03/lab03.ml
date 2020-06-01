(*Problem 1*)

let rec sumup a = 
      if a = 0 then 0
      else if a = 1 then 1
      else sumup(a-1) + a;;
      
(*Problem 2*)

let flip_pair 'a 'b = 
      if a = b then a b
      else
      temp = a
      a = b
      b = temp;;
      
let flip_list lst =
      match lst with
      | [] -> []
      | [h] -> [h]
      | [h::t] -> flip_pair[h] :: flip_pair[t]
      
      
(* Problem 3*)

 let rec destutter =
      if 
    function
      |  [] -> []
      |  [x] -> [x]
      |  (x::y::l) ->
           if (x=y) then destutter (x::l) else x :: destutter (y::l)

(* Problem 4*)

let sum_diffs lst = match [] with
      | [] -> 0
      | h::t -> h-t
      ;;
      
(* Problem 5 *)

let rec unzip lst = match lst with
      | [] -> ([],[])
      |(x,y)::tail -> 
            let (first, second) = unzip tail in
            (x::first, y::second)
            ;;
            
 (* Problem 6*)
 
 let rec hof n =
      let rec f =
            if n = 0 then 1
            else if n > 0 then
                  f = n-m(f(n-1))
      let rec m =
            if n = 0 then 1
            else if n > 0 then
                  m = n - f(m(n-1))
    ;;
