(*Solution to Problem 1*)

(*Part 1*)

let processOneList l = List.fold_left processWord [] l

let processWord list1 string =
  match list1 with
  | [] -> [(string,1)]
  | (h1,h2)::t -> if (h1 = string) then (string,h2+1))::t
                  else (h1,h2)::processWord t s

(*Part 2*)

 let assimilateWCList wcs wc1 =
                  List.fold_left assimilateWordCount wcs wcl

 let assimilateWordCount list1 (t1,t2) =
   match list1 with
   | [] -> [(t1,t2)]
   | (h1,h2)::t -> if (t1 = h1) then ((h1,(h2+t2))::t)
                   else if (h1 > t1) then ((t1,t2)::(h2,h2)::t) 
                   else ((h1,h2)::(assimilateWordCount t (t1,t2)))

(*Part 3*)

 let wordCounts ls =
   List.fold_left assimilateWCList [] (List.map processOneList ls)

(*Solution to Problem 2*)
   
 type 'a btree = Empty | Node of 'a * 'a btree * 'a btree

 (*helper function for the bst function*)
                               
 let rec insert f x t =
   match t with
   | Empty -> Node (x,Empty, Empty)
   | Node(x',l,r) -> if (f x x') then Node (x', insert f x l,r)
                     else Node (x',l,insert f x r)
                   
 type 'a bstree = { data : 'a btree; lss : 'a -> 'a -> bool;}

 let insert_bstree {data = t; lss = lss;} i =
   match t with
   | Empty -> {data = Node(i,Empty,Empty); lss = lss;}
   | Node(ind,l,r) -> {data = (insert lss i t); lss = lss;}

 (*Solution to Problem 3*)

 let rec cont_append l1 l2 c =
   match l1  with
   | [] -> (c l2)
   | (h::t) -> h :: cont_append t l2 (fun x -> c(h::x))
                        
 (*Solution to Problem 4*)

(*1.*)
type state = int * int * int

(*2.*)
let getX (x,y,z) = x
let getY (x,y,z) = y
let getZ (x,y,z) = z

let putX exp s =
   let (x,y,z) = s in (exp s,y,z)
let putY exp s =
   let (x,y,z) = s in (x,exp s,z)
let putZ exp s =
   let (x,y,z) = s in (x,y,exp s)

(*3.*)
                    
let seq stat1 stat2 =
        fun s -> (stat2 (stat1 s))
                        
let fibprog =
