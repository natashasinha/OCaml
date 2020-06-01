 (* Solution to Problem 4, Homework 7, CSci 2041, Spring 2020 *)

(* Some initial declarations identifying the signatures that should be
   satisfied by data and btree modules
*)

module type DATA =
   sig
     type item
     val leq : item * item -> bool
     val print : out_channel -> item -> unit
     val eq : item * item -> bool
   end

module type BTREE =
   sig
     type item
     type btree
     val insert : item * btree -> btree
     val print : out_channel -> btree -> unit
     val find : btree*item -> bool
     val initTree : unit -> btree
   end

(* Problem 4, Part 1 *)

(* The functor BTree is parameterized by a module that satisfies the DATA signature,  
as well as produces a module that satisfies the BTree signature. *)

module BTree(Data : DATA) : (BTREE with type item = Data.item) =
  struct
    type item = Data.item
    type color = R | B
    type btree = Empty
        | Node of color * item * btree * btree

    let balance t =
      match t with
      | ( Node(B,z,Node(R,x,Node(R,y,a,b),c),d) |
          Node(B,z,Node(R,y,a,Node(R,x,b,c)),d) |
          Node(B,y,a,Node(R,z,Node(R,x,b,c),d)) |
          Node(B,y,a,Node(R,x,b,Node(R,z,c,d))) ) ->
          Node(R,x,Node(B,y,a,b),Node(B,z,c,d))
      |  _ -> t

    let insert (d,t) =
      let rec ins t =
        match t with
        | Empty -> Node (R, d, Empty, Empty)
        | Node (c,d',l,r) ->
           if Data.leq (d,d') then balance (Node (c,d',ins l, r))
           else balance (Node (c,d', l,ins r))
      in match (ins t) with
         | Node (_,d,l,r) -> Node (B,d,l,r)
         | Empty -> raise (Invalid_argument "insert")

    let initTree () = Empty

    let rec find =
      function
      | (Empty,i) -> false
      | (Node(_,i',l,r),i) ->
         if Data.eq(i,i') then true
         else if Data.leq(i,i')
            then find (l,i)
            else find (r,i)

    let print outfile bt =
      let rec indent n =
        match n with
        | 0 -> ()
        | n -> (Printf.fprintf outfile "  "; indent (n-1)) in
      let rec print_aux n =
        function
        | Empty -> ()
        | Node (_,i,l,r) ->
           (print_aux (n+1) l;
            indent n; Data.print outfile i; Printf.fprintf outfile "\n";
        print_aux (n+1) r
       )
      in print_aux 0 bt

  end 


(* Problem 4, Part 2 *)

(* IntData is a module that satisfies the DATA signature and whose item type 
is recognizable externally as being identical to int. *)

module IntData : (DATA with type item = int) =
  struct 
    type item = int
    let leq ((p:item),(q:item)) : bool = p <= q
    let print out i = Printf.fprintf out "%d" i
    let eq ((p:item),(q:item)) : bool = p = q
  end

(* IntBTree module that realizes the BTREE signature with integer data. 
The functor BTree is used and applied to the IntData module.  *)
  
module IntBTree = BTree(IntData)
                

(* Problem 4, Part 3 *)

(*  StringData module that satisfies the DATA signature whose item type 
is recognizable externally as being identical to string.  *)

    module StringData : (DATA with type item = string) =
      struct
        type item = string
        let leq ((p:item),(q:item)) : bool = p <= q
        let print out i = Printf.fprintf out "%s" i
        let eq ((p:item),(q:item)) : bool = p = q
      end

(* StringBTree module that realizes the BTREE signature with string data. *)
      
   module StringBTree = BTree(StringData)
                                                  
          
