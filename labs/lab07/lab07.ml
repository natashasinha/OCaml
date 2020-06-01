(*Solution to Problem 1*)

let rec map2 f l1 l2 =
  match l1 with
  | [] -> []
  | (x::11) and (y::l2) -> (f x y) :: map2 f l1 l2
                      
(*Solution to Problem 2*)
                         
let rec map f l1 l2 =
  fun l2 ->
  match l1 with
      | [] -> l2
      | (h::t) -> h :: map f t l2


let rec append l =
  match l with
  | [] -> (fun l2 -> l2)
  | (h::t) ->
     let tail_appender = append t in
     (fun l2 -> h :: tail_appender l2)


(*Solution to Problem 3*)
       
 let rec accumulate f lst u =
      match lst with
      | [] -> u
      | (h::t) -> accumulate f t (f h u)

  let rec reduce f lst u =
     match lst with
     | [] -> u
     | (h::t) -> f h (reduce f t u)

  (*Part 1*)
   let union l1 l2 = accumulate f l1 l2

  (*Part 2*)
  unzip l = reduce (x,y) l (h::t)
                   
  (*Part 3*)
  let zip l1 l2 = map2 reduce l1 l2 

  (*Solution to Problem 4*)
                                   
  (*Part 1*)

  let fun compose f g = fun x -> (f (g x))
                                   
   (* type checking: 'a is assigned to compose
                     'a -> 'b 
                     'b -> 'b list
                     'b list -> 'c list*)

  (*Part 2 *)

  let rec forsome p l =
    match l with
    | [] -> false
    | (h::t) -> (p h) || (forsome p l)

  (* type checking: 
    'a list is assigned to forsome 
    'a list -> 'b list 
    'b list -> bool -- fails the type checking 
    'b list -> 'c list 
A list with one element, as well as a list containing an empty element will fail the type checking*)
