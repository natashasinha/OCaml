 (* Solution to Problem 3, Homework 7, CSci 2041, Spring 2020 *)

(* First some definitions that realize the new stream abstraction *)

type 'a stream = Stream of 'a stream_aux ref
and  'a stream_aux =
       | Evald of ('a * 'a stream)
       | UnEvald of (unit -> 'a * 'a stream)

(* Problem 3, Part 1 *)
   
  (* mkStream : (unit -> 'a * 'a stream) -> 'a stream
     nextStream : 'a stream -> 'a * 'a stream   *)

let mkStream f =
  let x = ref (UnEvald f)
  in Stream x

let rec nextStream (Stream f) =
  match !f with
  | Evald s1 -> s1
  | UnEvald s1 -> let (x,rst) = s1() in
                  (f := Evald(x,rst); nextStream (Stream f))

 (* either update the reference value with itself, or evaluate it *)

(* Problem 3, Part 2 *)

(*  natStream : int stream     *)   

let rec fromNStream n = mkStream (fun() -> (n,fromNStream(n+1)))

 let natStream = fromNStream 0
