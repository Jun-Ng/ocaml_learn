let p = (1, 4);;
let q = (1, '1');;
let census = [(1, 4); (2, 2); (3, 2); (4, 3); (5,1); (6, 2)];;
let	y = (1, [2; 3; 4]);;

let fst p = match p with (x, _) -> x;;
let snd p = match p with (_, y) -> y;;

let	rec	lookup x l =
	match l with
		| [] -> raise Not_found
		| (k, v)::t ->
			if k = x then v else lookup x t;;

let	rec add x v l =
	match l with
		| [] -> [(x,v)]
		| (k, kv)::t ->
			if k = x then (x, v) :: t else (k, kv) :: add x v t;;

let	rec	remove x l =
	match l with
		| [] -> []
		| (k, v)::t ->
			if k = x then t else (k, v) :: remove x t;;

let	rec	key_exists k l =
	try
		let _ = lookup k l 	in true
	with
		| Not_found -> false;;

(*
	Questions
	1. Write a function to determine the number of different keys in a dictionary.
	2. Define a function replace which is like add, but raises Not_found if the key is not already there.
	3. Write a function to build a dictionary from two equal length lists, one containing keys and another
		containing values. Raise the exception Invalid_argument if the lists are not of equal length.
	4. Now write the inverse function: given a dictionary, return the pair of two lists – the first containing
		all the keys, and the second containing all the values.
	5. Define a function to turn any list of pairs into a dictionary. If duplicate keys are found, the value
		associated with the first occurrence of the key should be kept.
	6. Write the function union a b which forms the union of two dictionaries. The union of two dictionaries is the dictionary containing all the entries in one or other or both. In the case that a key is
		contained in both dictionaries, the value in the first should be preferred.
*)

let rec length lst =
	let rec helper acc = function
		| [] -> acc
		| _::t -> helper (acc + 1) t 
	in helper 0 lst;;


(*
	1. Write a function to determine the number of different keys in a dictionary.

	[1, 2, 1]
	[1, 1, 1]
	[2, 2, 2]
	[(1,0);(2,0);(2,0);(2,0);(2,0);(2,0);(1,0)]
*)

let	rec	unique_keys lp =
	match lp with
		| [] -> 0
		| (k, _)::t -> 
			if key_exists k t then unique_keys t
			else 1 + unique_keys t;;

(*
	2. Define a function replace which is like add, but raises Not_found if the key is not already there.
*)

let	rec	replace k v lp =
	match lp with
		| [] -> raise Not_found
		| (k', v')::t ->
			if k' = k then (k, v) :: t
			else (k', v') :: replace k v t;;

(*
	3. Write a function to build a dictionary from two equal length lists, one containing keys and another
		containing values. Raise the exception Invalid_argument if the lists are not of equal length.


	[1;2; 3; 4] [2; 4; 6; 8]
*)

let	rec	build_lp lk lv =
	if length lk != length lv then raise (Invalid_argument "build_lp")
	else
		let	rec build lk lv =
			match lk, lv with
				| k::kt, v::vt -> (k, v) :: build kt vt
				| _ -> [] in
		build lk lv;;

(*
	4. Now write the inverse function: given a dictionary, return the pair of two lists – the first containing
		all the keys, and the second containing all the values.

	Simplest way
	function to get list of keys
	function to get list of values
	requires * 2 iteration

	Figure out
	function to just get list of keys & values
	* 1 iteration

	k, v -> [k] [v] split_kv_lp t
	
	(k, v) (k, v) (k, v)
	k ::k ::k v v v

	kl vl
	(k, v)::t -> 

	(k :: k :: [], v :: v :: [])

	[] []

	

*)

let	ft_rev l =
	let	rec helper acc = function
		| [] -> acc
		| h::t -> helper ([h] @ acc) t
	in helper [] l;;

let	rec split_kv_lp lp =
	let	rec	split lk lv lp =
		match lp with
			| [] -> (lk, lv)
			| (k, v)::t -> split (k :: lk) (v :: lv) t in
	split [] [] (ft_rev lp);;

(*
	5. Define a function to turn any list of pairs into a dictionary. If duplicate keys are found, the value
		associated with the first occurrence of the key should be kept.

	list of pairs to dict
	basically just remove dupes of keys

	[1; 2; 1]

	revervse list remove of key still exists

	2 :: 1 

	[(1, 2); (2, 3); (2, 4); (1, 4); (3, 5)]
*)

let	rec	list_to_dict l =
	let	rec convert l =
		match l with
			| [] -> []
			| (k, v)::t ->
				if key_exists k t then convert t
				else (k, v) :: convert t in
	ft_rev (convert (ft_rev l));;

(*
	6. Write the function union a b which forms the union of two dictionaries. The union of two dictionaries is the dictionary containing all the entries in one or other or both. In the case that a key is
		contained in both dictionaries, the value in the first should be preferred.

	[(1, 1); (2, 1)] [(2, 2); (1, 2)]

*)

let	rec union a b =
	list_to_dict (a @ b);;