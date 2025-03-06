(* let rec evens l =
    match l with
		| _::m::t ->  m :: evens t
		| _ -> [];; *)

(* let	rec counts_true l n =
	match l with
		| h::t -> counts_true t (n + if h then 1 else 0 )
		| [] -> n;; *)

(* let	rec	ft_rev l =
	match l with
		| h::t -> (ft_rev t) @ [h]
		| [] -> [];; *)

let	ft_rev l =
	let	rec helper acc = function
		| [] -> acc
		| h::t -> helper ([h] @ acc) t
	in helper [] l;;

(**
	[palindrome l]

	Since it didn't specify in making the shortest palindrome possible
		just l @ rev l is enough

	To make the shortest
	1.	find the longest palindromic prefix
	2.	get the remaining suffix
	3.	reverse the suffix
	4.	append it to the end
*)
let	rec	palindrome l = l @ (ft_rev l);;

let	rec drop n l =
	if n = 0 then l else
		match l with
			| _::t -> drop (n - 1) t
			| [] -> l;;

let	rec	is_palindrome l = l = ft_rev l;;

let	drop_last l = 
	let	helper = function
		| [] -> []
		| _::t -> ft_rev t
	in helper (ft_rev l);;

let	rec	member element list =
	match list with
		| h::t -> if h = element then true else member element t
		| [] -> false;;
	
(**
	[make_set l]

	given a list,
	returns a list that contains
	all elements of the original list,
	but no dupes

	[1 1 2 3]
	[1 2 3]
	or
	[3 2 1]
	or
	[2 3 1]

	order doesn't matter
*)

let	rec	make_set list =
	match list with
		| h::t -> if (member h t) then make_set t else make_set (t @ [h])
		| [] -> [];;

