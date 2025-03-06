(*
	Questions
		1. Design a new type rect for representing rectangles. Treat squares as a special case.
		2. Now write a function of type rect → int to calculate the area of a given rect.
		3. Write a function which rotates a rect such that it is at least as tall as it is wide.
		4. Use this function to write one which, given a rect list, returns another such list which has the
			smallest total width and whose members are sorted narrowest first.
		5. Write take, drop, and map functions for the sequence type.
		6. Extend the expr type and the evaluate function to allow raising a number to a power.
		7. Use the option type to deal with the problem that Division_by_zero may be raised from the
			evaluate function.
*)

type 'a option = None | Some of 'a;;
type 'a sequence = Nil | Cons of 'a * 'a sequence;;


let	rec length l =
	let	rec	tail acc = function
		| [] -> acc
		| _::t -> tail (acc + 1) t in
	tail 0 l
;;


let	rec	take n l =
	if n = 0 then [] else 
		match l with
			| h::t -> h :: take (n - 1) t
			| [] -> [];;

let	rec drop n l =
	if n = 0 then l else
		match l with
			| _::t -> drop (n - 1) t
			| [] -> l;;

let	rec	merge cmp x y =
	match x, y with
		| [], l -> l
		| l, [] -> l
		| xh::xt, yh::yt ->
			if cmp xh yh
				then yh :: merge cmp yt (xh :: xt)
				else xh :: merge cmp xt (yh :: yt);;

let rec msort cmp l =
	match l with
		| [] -> []
		| [a] -> [a]
		|  l -> (
			let length_of_list = length l in
			let middle = length_of_list / 2 in
			merge cmp (msort cmp (take middle l)) (msort cmp (drop middle l))
		);;

(* Q1 *)
type rect =
	| Square of int
	| Rect of int * int;;
	

(* Q2 *)
let	area = function
	| Rect(l, h) -> l * h
	| Square(s) -> s * s;;

(* Q3 *)
let	rotate = function
	| Square(s) -> Square(s)
	| Rect(l, h) -> Rect(h, l);;

(* Q4
	1.	rect list
	2.	returns a list
		1.	sorted
			1.	smallest total width = width * 2


	[1; 2; 3]
	
	[1; 3; 2; 2]

	1 :: [3 2 2]
	1 :: 3 > 2 [2]
	1 :: 2 :: 3 > 2
	1 :: 2 :: 2
*)

let	get_width = function
	| Rect(w, _) -> w
	| Square(s) -> s;;

let	sort_width l =
	msort (fun x y -> get_width x > get_width y) l;;


let	list = [Rect(10, 2); Square 5; Rect(9, 2); Square 4; Square 2];;

(*
	Q5
*)

let	rec	take_seq n seq =
	if n = 0 then Nil else
		match seq with
			| Nil -> Nil
			| Cons(h, t) -> Cons(h, take_seq (n - 1) t);;

let	rec	drop_seq n seq =
	if n = 0 then seq else
		match seq with
			| Nil -> Nil
			| Cons(_, t) -> drop_seq (n - 1) t;;

let	rec	map_seq f seq =
	match seq with
		| Nil -> Nil
		| Cons(h, t) -> Cons(f h, map_seq f t);;
	
(* Q6 & Q7 *)

type	expr =
	| Num of int
	| Add of expr * expr
	| Subtract of expr * expr
	| Multiply of expr * expr
	| Divide of expr * expr
	| Power of int;;

let rec power base exponent =
	if exponent = 0 then 1
	else base * power base (exponent - 1);;

let	rec	evaluate e = 
	let	rec	process e =
		match e with
			| Num n -> n
			| Add(e, e') -> process e + process e'
			| Subtract(e, e') -> process e - process e'
			| Multiply(e, e') -> process e * process e'
			| Divide(e, e') -> process e / process e'
			| Power n -> (power n n) in
	try Some (process e) with
		| Division_by_zero -> None;;
