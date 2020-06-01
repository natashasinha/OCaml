(* Solution to Problem 3, Homework 2, CSci 2041, Spring 2020 *)

(*The option type will be used when there is not an entry in the database that corresponds to the input name. The option type has 2 constructors - None and Some. When there is not a matching name in the database the None constructor will be referenced and if there is some matching input then the program will fall into the Some category.*)

type 'a option = None | Some of 'a
              
(*The user will initially enter a list of data in the form of tuples. Each
entry will contain a string name, string phone number, and a float salary value.
The user will also enter a name of the person they want to look up in the 
database. If the name is found in the database then the corresponding
salary will be returned, otherwise None will be returned. *)

let rec find_salary lst name =
  match lst with
  | [] -> None
  | (h, m, t):: tail -> if h = name then (Some t) else find_salary tail name


 (* This has a similar format to the function above. In this case, the user
will enter a string name and if the name is found in the database, the 
corresponding phone number will be returned. If the name is not found
in the database then None will be returned.*)
                      
let rec find_phno lst name =
  match lst with
  | [] -> None
  | (h, m, t):: tail -> if h = name then (Some m) else find_phno tail name
