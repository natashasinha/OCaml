(* Solution to Problem 1, Homework 2, CSci 2041, Spring 2020 *)

(* Solution to Part 1 *)

(*This function is well-typed. The function takes in a tuple containing
2 lists of any type ('a list * 'b list). It then assigns the first element (l1)
to the variable x and the second element (l2) to the variable y. The function 
then resursively calls itself and appends the second list to the first,
therefore creating a new zipped list.*)

(* Solution to Part 2 *)

(*This function is not well-typed. In the second instance the function
is attempting to make a recursive call that appends a list to a non-list,
therefore causing a type error. Since the head is of type 'a, an error
occurs because the call is expecting an append to a list and not
an element.
(*Error message: "This expression has type 'a list but an expression
was expected of type 'a"*)
