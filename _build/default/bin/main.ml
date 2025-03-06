let	rec	take n l =
	if n < 0 then raise (Invalid_argument "take")
	else if n <= 0 then [] else 
		match l with
			| h::t -> h :: take (n - 1) t
			| [] -> [];;
let	rec drop n l =
	if n < 0 then raise (Invalid_argument "drop")
	else if n = 0 then l else
		match l with
			| _::t -> drop (n - 1) t
			| [] -> l;;

let	safe_divide x y =
	try x / y with
		Division_by_zero -> 0;;

let	rec	last l =
	match l with
		| [] -> raise Not_found
		| [x] -> x
		| _::t -> last t;;

(*
	Questions
	1. Write a function smallest which returns the smallest positive element of a list of integers. If there
	is no positive element, it should raise the built-in Not_found exception.
	2. Write another function smallest_or_zero which uses the smallest function but if Not_found is
	raised, returns zero.
	3. Write an exception definition and a function which calculates the largest integer smaller than or
	equal to the square root of a given integer. If the argument is negative, the exception should be
	raised.
	4. Write another function which uses the previous one, but handles the exception, and simply returns
	zero when a suitable integer cannot be found.
	5. Comment on the merits and demerits of exceptions as a method for dealing with exceptional
	situations, in contrast to returning a special value to indicate an error (such as -1 for a function
	normally returning a positive number).
*)

(*
	1. Write a function smallest which returns the smallest positive element of a list of integers. If there
		is no positive element, it should raise the built-in Not_found exception.
*)

let	rec smallest l =
	let rec find current_smallest l =
		match l with
			| [] -> if current_smallest >= 0 then current_smallest else raise Not_found
			| h::t -> if h < current_smallest then find h t else find current_smallest l 
		in
	match l with
		| h::t 
	