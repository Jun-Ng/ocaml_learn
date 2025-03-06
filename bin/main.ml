type color = 
	| Red 
	| Green 
	| Blue 
	| Yellow
	| RGB of int * int * int;;

let col = Blue;;
let cols = [Red; Red; Green; Yellow; RGB (150, 0, 255)];;
let colpair = ('R', Red);;

let	components c =
	match c with
		| Red -> (255, 0, 0)
		| Green -> (0, 255, 0)
		| Blue -> (0, 0, 255)
		| Yellow -> (255, 255, 0)
		| RGB (r, g, b) -> (r, g, b);;

type 'a option = None | Some of 'a;;

let nothing = None;;
let number = Some 50;;

let	numbers = [Some 12; None; None; Some 2];;

let	word = Some "cake";;

let	rec	lookup_opt x l =
	match l with
		| [] -> None
		| (k, v)::t -> if x = k then Some v else lookup_opt x t;;

(*
	New types may also be recursively defined
*)
(* 
	This is basically a list type
	[] = Nil = (type = 'a sequence)
	Cons (1, Nil)  = int list
	Cons('a', Nil) = char list
	...

	Looking at the code below
	you can tell why it takes longer to access deeper elements in list for ocaml

	Recursively-defined type
*)
type 'a sequence = Nil | Cons of 'a * 'a sequence;;

(* list *)
let	int_seq = Cons (2, Cons (4, Nil));;
let	char_seq = Cons ('a', Cons ('b', Nil));;

let	color_seq = Cons (RGB(10, 10, 10), Cons(Yellow, Nil));;

(*
	Creating basic functions for our sequences above
*)

let	rec	append x y =
	match x with
		| Nil -> y
		| Cons (h, t) -> Cons (h, (append t y));;

let	rec	length l =
	match l with
		| Nil -> 0
		| Cons(_, t) -> 1 + length t;;

(*
	Creating A Type for Mathematical Expressions
*)

type	expr =
	| Num of int
	| Add of expr * expr
	| Subtract of expr * expr
	| Multiply of expr * expr
	| Divide of expr * expr
	| Power of int

let rec power base exponent =
	if exponent = 0 then 1
	else base * power base (exponent - 1);;

let	rec	evaluate e =
	match e with
	| Num n -> n
	| Add(e, e') -> evaluate e + evaluate e'
	| Subtract(e, e') -> evaluate e - evaluate e'
	| Multiply(e, e') -> evaluate e * evaluate e'
	| Divide(e, e') -> evaluate e / evaluate e'
	| Power e -> power e e;;
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
