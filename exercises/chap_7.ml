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
*)

(*
	1. Write a function smallest which returns the smallest positive element of a list of integers. If there
		is no positive element, it should raise the built-in Not_found exception.
*)

let	rec smallest l =
	let rec find current_smallest l =
		match l with
			| [] -> if current_smallest > 0 then current_smallest else raise Not_found
			| h::t ->
				if (h > 0 && h < current_smallest) ||  current_smallest < 0 then find h t 
				else find current_smallest t 
	in
	match l with
		| [] -> raise Not_found
		| h::t -> find h t;;


(*
	2. Write another function smallest_or_zero which uses the smallest function but if Not_found is
		raised, returns zero.
*)

let	rec smallest_or_zero l =
	try smallest l with
		Not_found -> 0;;

(*
	3. Write an exception definition and a function which calculates the largest integer smaller than or
		equal to the square root of a given integer. If the argument is negative, the exception should be
		raised.

	1.	largest integer <= square root of input
	2.	if input is neg, raise exception

	sqrt function
	1.	ocaml has a sqrt function for floats
	2.	building one for integers
	3.	


	start with 1

	41 = (1 + 81 / 1) /2
	21 = (41 + 81 / 41) /2
	12 = (21 + 81 / 21) /2
	9 = (12 + 81 / 12) /2
	9 = (9 + 81 /9) /2


	8 = (1 + 8/1)/2
	3 = (4 + 8/4)/2
	2 = (3 + 8/3)/2

*)

let	rec	sqrt_int n =
	let	rec acc guess n =
		let	next_guess = (guess + (n / guess)) / 2 in
		if next_guess * next_guess = n then next_guess
		else if next_guess < (n / 2) then raise (Invalid_argument "sqrt_int")
		else acc next_guess n

	in
	acc ((1 + n)/2) n;;
	


exception NotPositive
let	rec ln_smaller_sq n = 
	if n < 0 then raise NotPositive 
	else (sqrt_int n);;

(*
	4. Write another function which uses the previous one, but handles the exception, and simply returns
		zero when a suitable integer cannot be found
*)

let	rec ln_smaller_sq_use n = 
	try ln_smaller_sq n with 
		| Invalid_argument _ -> 0
		| NotPositive -> 0;;
