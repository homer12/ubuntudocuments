let pi = 4.0 *. (atan 1.0);;

(* Q1 *)
let volume_of_cube (a: float) =
  a ** 3.0;;

(* Q2 *)
let edgelen_of_cube (volume: float) =
  volume ** (1.0 /. 3.0);;

(* Q3 *)
let volumes_of_cubes (edgelens: float list): float list =
  List.map volume_of_cube edgelens;;

(* Q4 *)
type number = Int of int | Float of float | Error;;
type sign = Positive | Negative;;

let sign_int (n: int): sign =
  if n >= 0 then Positive
  else Negative;;

(* Didn't check if n1 < 0 or n2 < 0*)

let rec ack (n1: int) (n2: int): int =
	if n1 = 0 then n2+1
	else if n2 = 0 then ack (n1-1) 1
	else ack (n1-1) (ack n1 (n2-1));;

(* Note: div_num (Int 4) (Int 2) should give (Int 2) rather than (Float 2.0) *)

let rec wc (words: string list) (word: string): int =
  match words with
		[] -> 0
		| head :: rest ->
			if head = word then 1 + wc rest word
			else wc rest word;;
