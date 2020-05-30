(* Problem 1 in Homework 4, CSci 2041, Spring 2020 *)

(*1a. The function makePairLists takes in a person of type string and a list of their friends,
 all of which are of type string. It then takes the person and the list and generates a 
new list containing pairs, where each pair contains the pair and a friend from the 
input list. One thing to note about the output is that it is in lexicographic order
meaning that the person whose name is relatively first in the alphabet will be displayed first. *)

let makePairLists p fl = List.map (fun x -> if x<p then ((x,p),fl)
                                          else ((p,x),fl)) fl

(*1b. The function makeAllPairLists takes the same input as above, but this function
returns an output generated specifically by the mapping function. The output is a list
of lists containing all of the potential pairs along with the associated friend lists. *)

let makeAllPairLists l = List.map (fun x -> let (s,l1) = x in makePairLists s l1) l

(*2a. The function intersect finds the intersections between two sets of friends. 
A helper function is used to determine if an element in one friend list matches
an element in the friend list being compared. If a match is found, the helper 
function returns true. If not, the helper function is then recursively called
again to check the next element/friend. The insert function then works by calling the
 helper function. If the helper function returns true the common friend is appended to the list of
common friends. If the friend is not found to be common then it is not appended. *)

let rec appears a listHelp =
  match listHelp with
  | [] -> false
  | h::t -> if h = a then true else appears a t
          
let intersect l1 l2 = List.fold_right (fun x y -> if(appears x l2) then x::y
                                                  else y) l1 []

(*2b. The function addOnePair takes one pair of friends and a list of the potential common friends.
 It then adds this information to the previously accumulated unfinished list of common friends. If this
 input pair already exists within the list then the two perspectives will be combined. If the pair does
 not already exist within the list then it is added to it, and the result is the list containing the pairs
 and the lists of common friends, with the added pair/list input. *)

let rec addOnePair ((pair1,pair2),lp) l1 =
  match l1 with
  | [] -> ((pair1,pair2),lp)::[]
  | ((x1,x2),lx)::t -> if (x1 = pair1 && x2 = pair2) then ((pair1,pair2), intersect lp lx)::t
                       else ((x1,x2),lx)::addOnePair ((pair1,pair2),lp) t
                     
(* 2c. The function addAllPairs takes the list that associates all of the friends of a person with
 each pair of the person, with their friends, and the unfinished list from above, with the goal of 
making a new more aggregated output. It essentially brings all of the pairs and lists together into
 one large "directory" list. *)

let addAllPairs ppls l = List.fold_left (fun x y -> addOnePair y x) l ppls

(*3. The function commonFriends takes the mapping and aggregation operations and puts them together 
in a map-reduce combination. It uses the List.fold function to do this. The result is a more condensed
 directory from the previous problem. *)
                       
let commonFriends l = List.fold_right addAllPairs (makeAllPairLists l) []   

(* The example friends list used in the problem writeup is provided via the
   let binding below *)

let friendsList = 
   [ ("a", ["b"; "c"; "d"]);
     ("b", ["a"; "c"; "d"; "e"]);
     ("c", ["a"; "b"; "d"; "e"]);
     ("d", ["a"; "b"; "c"; "e"]);
     ("e", ["b"; "c"; "d"]) 

