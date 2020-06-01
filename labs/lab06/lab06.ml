(*Solution to Problem 1*)

type option = None | Some of 'a
                           
type empItemTy = {name: string;
                   phone: string;
                   salary: float};;

let smalldb =
  [ {name = "John"; phone = "x3456"; salary = 50.1};
    {name = "Jane", phone = "x1234", salary = 107.3};
    {name = "Joan", phone = "unlisted", salary = 12.7}]


let find_salary lst name =
  match lst with
  | [] -> None
  | {name = n'; salary = s}::tail
           if n' = name then s
           else find_salary tail name
  
let find_phno lst name =
  | [] -> None
  | {name = n'; phone = p}::tail
           if n' = name then p
           else find_phno tail name

(* Solution to Problem 2*)
  
type myItem = Item of 'a
            | Nested of 'a myItem list
                      
type nestedlist = 'a myItem list
                  
let nestedlist intlist1 = [Item 1;[Nested 2;[Nested3];Nested 4];[Nested 5;Nested 6;[Nested 7]]]


let flatten list =
  match list with
  | [] -> None
  | (h::t) -> (h:: flatten t)

            
(* Solution to Problem 3*)

(*Part 1*)
    
type form = Const of string
          | Not of form
          | And of (form * form)
          | Or of (form * form)
                

let form1 = Not(Not(p))(And(q))Or((Not(q))And(r))
let form2 = Not(Not(p(Or(Not(q)))Or(Not((Not(q))Or(r)))))

(*Part 2*)

type option = None | Some of 'a
                           
let rec neg f =
     match f with
     | Const n -> f
     | And(a,b) -> Or(neg a, neg b)
     | Or(a,b) -> And(neg a, neg b)
     | Not a -> neg a
                        
        in
    
    let nnf f =
      match f with
      | f -> neg f
      | _ -> None
