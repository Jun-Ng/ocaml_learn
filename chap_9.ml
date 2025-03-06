let add x y = x + y;;
let f = add 6;;

let rec map f l =
	match l with
		| [] -> []
		| h::t -> f h :: map f t;;

let	rec	mapl f l =
	match l with
		| [] -> []
		| h::t -> map f h :: mapl f t;;

(* partial application mapl *)
let	mapl f l = map (map f) l;;
(* can even go further since map f l*)
let	mapl f = map (map f);;
map (add 6) [10; 20; 30];;
map ((+) (-2)) [10; 20; 30];;

mapl (( * ) 2) [[1;2];[1;2]];;

(* Multiple argument function *)
let	add x y = x + y;;
(* To better understand the structure of multiple-argument functions *)
let add = fun x -> fun y -> x + y;;

(*
	f x y = x -> y -> c
	
	Thus a function can take an argument of a, and return a function of (b -> c)
	f x y = x -> (y -> c)

	let f x y 
	is just a shorthand for
	let f = fun x -> fun y -> ...
*)

(*
	Questions
		1. Rewrite the summary paragraph at the end of this chapter for the three argument function g a b c.
		2. Recall the function member x l which determines if an element x is contained in a list l. What is its
			type? What is the type of member x? Use partial application to write a function member_all x ls
			which determines if an element is a member of all the lists in the list of lists ls.
		3. Why can we not write a function to halve all the elements of a list like this: map (( / ) 2) [10; 20;
			30]? Write a suitable division function which can be partially applied in the manner we require.
		4. Write a function mapll which maps a function over lists of lists of lists. You must not use the let
			rec construct. Is it possible to write a function which works like map, mapl, or mapll depending
			upon the list given to it?
		5. Write a function truncate which takes an integer and a list of lists, and returns a list of lists, each
			of which has been truncated to the given length. If a list is shorter than the given length, it is
			unchanged. Make use of partial application.
		6. Write a function which takes a list of lists of integers and returns the list composed of all the first
			elements of the lists. If a list is empty, a given number should be used in place of its first element.
*)

(*
	Question 1
		1. Rewrite the summary paragraph at the end of this chapter for the three argument function g a b c.

	f x y z = x -> y -> z -> r
	f x y z = x -> (y -> z -> r)
	f x y z = x -> y (z -> r)

	Theory proven below
*)

let	sum_three x y z = x + y + z;;
let fx = sum_three 1;;
let	fy = fx 2;;
let fz = fy 3;;

(*
	Question 2

	2. Recall the function member x l which determines if an element x is contained in a list l. What is its
		type? What is the type of member x? 
		Use partial application to write a function member_all x ls
		which determines if an element is a member of all the lists in the list of lists ls.

	x -> x list -> Bool

	2 -> [1; 2; 3] -> true;;

	x -> x list list -> bool

	member_all 2 [[1; 2; 3];[1; 2;];[1; 2; 3]];;
	member_all 3 [[1; 2; 3];[1; 2;];[1; 2; 3]];;
	member_all 0 [[1; 2; 3];[1; 2;];[1; 2; 3]];;
	member_all 0 [];;
	member_all 1 [[];[]];;

	member
	x [] -> false
	x [y, z] -> false
	x [x,y,z] -> true

	member_all
	x [] -> true
		Description of function "x is a member of all lists in the collection."
		If there are no lists in the collection, there can't be any list that doesn't contain x
		so the statement is vacuously true.
		
		But also List.for_all returns true if the list is empty. so I'm conviced that the theory above is applied commonly in code?
	x [[]] -> false
	x [[x]; [x; y]] -> true
	x [[y]; [z]] -> false
*)

let	rec	member x l =
	match l with
		| [] -> false
		| h::t -> if h = x then true else member x t;;

let	rec	member_all x l =
	List.for_all (member x) l;;

let	rec	member_all x l =
	match l with
		| [] -> false
		| h::t -> (member x h) && (member_all x t);;
		
(*
	Question 3
	3. Why can we not write a function to halve all the elements of a list like this: map (( / ) 2) [10; 20;
		30]? Write a suitable division function which can be partially applied in the manner we require.
*)

let	divide_by x y = y / x;;

map (divide_by 2) [10; 20; 30];;

(*
	Q4

	4. Write a function mapll which maps a function over lists of lists of lists. You must not use the let
		rec construct. Is it possible to write a function which works like map, mapl, or mapll depending
		upon the list given to it?	

	
	f -> (x -> y) -> x list list list -> y list list list

	mapll (( * ) 2) [[[1; 2; 3];[1; 2; 3];[1; 2; 3];];[[1; 2; 3];[1; 2; 3];[1; 2; 3];]]
	
*)

let	mapll f lll = map (mapl f) lll;;

mapll (( * ) 2) [[[1; 2; 3];[1; 2; 3];[1; 2; 3];];[[1; 2; 3];[1; 2; 3];[1; 2; 3];]];;

(*
	Q5

	5. Write a function truncate which takes an integer and a list of lists, and returns a list of lists, each
		of which has been truncated to the given length. If a list is shorter than the given length, it is
		unchanged. Make use of partial application.

	x -> list list -> list (list length <= x)

	0 [[1; 2; 3];[1; 2; 3]] -> [[];[]]
	1 [[1; 2; 3];[1; 2; 3]] -> [[1];[1]]
	5 [[1; 2; 3];[1; 2; 3]] -> [[1; 2; 3];[1; 2; 3]]
	3 [[1; 2; 3];[1; 2; 3]]

*)

let	rec length l =
	let	rec	tail acc = function
		| [] -> acc
		| _::t -> tail (acc + 1) t in
	tail 0 l
;;


let	rec drop n l =
	if n <= 0 then l
	else match l with
		| [] -> l
		| _::t -> drop (n - 1) t;;

let	rec	take n l =
	if n <= 0 then []
	else match l with
		| [] -> l
		| h::t -> h :: take (n - 1) t;;

let	rec	truncate x ls = map (take x) ls;;			

(*
	Q6

	6. Write a function which takes a list of lists of integers and returns the list composed of all the first
		elements of the lists.
		If a list is empty, a given number should be used in place of its first element.	
	int -> int list list -> int list

	first_ll_int 1 [[]; [1; 2]] -> [1; 1] || [2; 1];;
	first_ll_int 2 [[1; 2]; []];;
*)

let	get_replace_first n l =
	match l with
		| [] -> n
		| h::_ -> h;;

let	rec	first_ll_int n l = map (get_replace_first n) l;;