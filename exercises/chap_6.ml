(**
	Questions
		1. Write a simple recursive function calm to replace exclamation marks in a char list with periods. For
			example calm ['H'; 'e'; 'l'; 'p'; '!'; ' '; 'F'; 'i'; 'r'; 'e'; '!'] should evaluate to
			['H'; 'e'; 'l'; 'p'; '.'; ' '; 'F'; 'i'; 'r'; 'e'; '.']. Now rewrite your function to use
			map instead of recursion. What are the types of your functions?
		2. Write a function clip which, given an integer, clips it to the range 1 . . . 10 so that integers bigger
			than 10 round down to 10, and those smaller than 1 round up to 1. Write another function cliplist
			which uses this first function together with map to apply this clipping to a whole list of integers.
		3. Express your function cliplist again, this time using an anonymous function instead of clip.
		4. Write a function apply which, given another function, a number of times to apply it, and an initial
			argument for the function, will return the cumulative effect of repeatedly applying the function. For
			instance, apply f 6 4 should return f (f (f (f (f (f 4)))))). What is the type of your function?
		5. Modify the insertion sort function from the preceding chapter to take a comparison function, in the
			same way that we modified merge sort in this chapter. What is its type?
		6. Write a function filter which takes a function of type α → bool and an α list and returns a list of
			just those elements of the argument list for which the given function returns true.
		7. Write the function for_all which, given a function of type α → bool and an argument list of type
			α list evaluates to true if and only if the function returns true for every element of the list. Give
			examples of its use.
		8. Write a function mapl which maps a function of type α → β over a list of type α list list to produce
			a list of type β list list
*)

(*
	Write a simple recursive function calm to replace exclamation marks in a char list with periods. For
		Input	['H'; 'e'; 'l'; 'p'; '!'; ' '; 'F'; 'i'; 'r'; 'e'; '!']
		Output	['H'; 'e'; 'l'; 'p'; '.'; ' '; 'F'; 'i'; 'r'; 'e'; '.'] 
	Now rewrite your function to use map instead of recursion. What are the types of your functions?
*)

let	rec map f l =
	match l with
		| [] -> []
		| h::t -> f h :: map f t;;
let	calm l =
	map (fun x -> if x = '!' then '.' else x) l;;

(*
	2. Write a function clip which, given an integer, clips it to the range 1 . . . 10 so that integers bigger
		than 10 round down to 10, and those smaller than 1 round up to 1. 
	2a. Write another function cliplist
		which uses this first function together with map to apply this clipping to a whole list of integers.

	if < 1 = 1
	else if > 10 = 10
	else n = n

	3. Express your function cliplist again, this time using an anonymous function instead of clip.
*)

let	clip n =
	if n < 1 then 1
	else if n > 10 then 10
	else n;;

let	cliplist l =
	map (fun n ->
			if n < 1 then 1
			else if n > 10 then 10
			else n) l;;

(*
	4. Write a function apply which, given another function, a number of times to apply it, and an initial
		argument for the function, will return the cumulative effect of repeatedly applying the function. For
		instance, apply f 6 4 should return f (f (f (f (f (f 4)))))). What is the type of your function?
*)

let	rec apply f times arg =
	if times > 0 then apply f (times - 1) (f arg)
	else arg;;

(*
	5. Modify the insertion sort function from the preceding chapter to take a comparison function, in the
		same way that we modified merge sort in this chapter. What is its type?
*)

let rec isort cmp l =
	let rec insert x = function
		| [] -> [x]
		| h::t -> if cmp x h then x :: h :: t else h :: insert x t in
	match l with
		| [] -> []
		| h::t -> insert h (isort cmp t);;
;;

(*
	6. Write a function filter which takes a function of type α → bool and an α list and returns a list of
		just those elements of the argument list for which the given function returns true.

	1.	takes a function typ of a -> bool & a list
	2.	returns a list of the elements where the function returns true when applied to it

	[1; 2; 1; 2]

	1 :: filter [2 1 2]

*)

let	rec	filter func l =
	match l with
		| h::t -> if func h then h :: filter func t else filter func t
		| [] -> [];;

(*
	7. Write the function for_all which, given a function of type α → bool and an argument list of type
	α list evaluates to true if and only if the function returns true for every element of the list. Give
	examples of its use.

	params
	1.	func type α → bool
	2.	α list

	output
	true -> if all of the elements in the list returns true when func a
	false -> when not true
*)

let	rec	for_all func l =
	match l with
		| [] -> true
		| h::t -> if func h then for_all func t else false;;

(*
	8. Write a function mapl which maps a function of type α → β over a list of type α list list to produce
		a list of type β list list

	maps a function of type a -> B

	over a list of type a list list
	output
	type B list list
*)

let	rec	mapl func l =
	match l with
		| h::t -> map func h :: mapl func t
		| [] -> [];;