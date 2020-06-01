(*Problem 1*)

type 'a btree =
    Empty
  | Node of 'a * 'a btree * 'a btree


let rec maxTree btree =
  match btree with
  | Empty -> None
  | (a, b, c) ->
     match maxTree c with
     | None -> Some a
     | Some b -> Some b 
  
 let minTree btree =
   match btree with
   | Empty -> None
   | (a,b,c) ->
      match maxTree b with
      | None -> Some a
      | Some b -> Some b

let rec isSearchTree t =
   let bigger i j =
     match j with
     | None -> true
     | (Some j') -> i >= j' in
   let smaller i j =
     match j with
     | None -> true
     | Some j' -> j' >= i in
   match t with
   | Empty -> true
   | Node (i,l,r) ->
         isSearchTree l && isSearchTree r &&
           (bigger i (maxTree l)) && (smaller i (minTree r))

(*test definitions*)

 (* let t1 = (1,2,5(3, 2,1))
  let t2 = (4,3,2(empty, empty))
  let t3 = (1,9,6,3,5,4(3,2,1),(4,5,2)) *)
       

(* Problem 2*)

type ty = IntTy | BoolTy
                
let rec typeof expr =
  match expr with
  | typeof(e1) != typeof(e2) -> None
  | typeof(e1) == IntTy ->
       if typeof(e2) == IntTy then IntTy else None
  | typeof(e1) == BoolTy ->
       if typeof(e2) == BoolTy then BoolTy else None

  let welltyped expr =
    let result = typeof(expr)
        match result with
          | IntTy -> True
          | BoolTy -> True
          | None -> False

 (* Test definitions*)

 (* let e1 = Cond (Lss (Int 10, Plus (Int 5, Int 7)),
                  Int 5, Int 7)
  let e2 = Cond (Lss (Int 10, Plus (Int 5, Int 7)),
                  Int 5, True)
  let e3 = Cond (And (Lss (Int 10, Plus (Int 5, Int 7)), True),
                  False, True)*)      
      
  (*Problem 3*) 

  type ocamlTy = IntTy' | BoolTy'|
                 String of string | ty of ty |
                 ty1 of ty1 |ty2 of ty2 |
  
