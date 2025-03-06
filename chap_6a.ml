let	rec length l =
	let	rec	tail acc = function
		| [] -> acc
		| _::t -> tail (acc + 1) t in
	tail 0 l
;;


(** Make it  tail recursive *)

let	rec	map f l =
	match l with
		| [] -> []
		| h::t -> f h :: map f t;;


(**
	1.	either index it
	2.	new array with result of function
	3.	length - 1

	h::t

	tail_map f (t @ f h) (length - 1)?

	[2] @ [f 1]
	[f 1] @ [f 2]

*)

let	double y = y * 2;;
let	halve x = x / 2;;

let	is_even	x = x mod 2 = 0;;

(**
	you can declare the function like this 
	instead of declaring it externally like above if you're using it once
*)
let	is_evens l =
	map (fun x -> x mod 2 = 0) l;;

let	greater x y =
	x >= y;;

let	lesser x y =
	x <= y;;

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

(**
	msort (>=) []
	msort (<=) []

	works as well
*)

