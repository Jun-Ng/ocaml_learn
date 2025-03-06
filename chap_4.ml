(*  Lists 
    [1; 2; 3]
	head = 1
	tail = [2; 3]

	lists with no elements are called "nil"
	nil: []

	lists with a single element
	[5]
	head = 5
	tail = []

	The :: aka "cons" = .shift
	O(N) = 1

	0 :: [1; 2]
	=> [0; 1; 2]

	@ aka "append"
	O(N) = n

	[1; 2] @ [3; 4; 5]
	=> [1; 2; 3; 4; 5]


*)

let	ft_isnil list =
	match list with
		[] -> true
		| _ -> false;;

Printf.printf "ft_isnil [] %s [1] %s\n"
	(string_of_bool (ft_isnil [])) (string_of_bool (ft_isnil [1]))


(**	
	[ft_list_length list] calculates the length of list 

	first we match with empty list
	then we take the tail of the list
*)
let	rec	ft_list_length list =
	match list with
		| [] -> 0
		| _::t -> 1 + ft_list_length t;;

Printf.printf "%d\n" (ft_list_length [1; 2]);;

(** [ft_sum_list list] sums the list *)
let	rec	ft_sum_list list =
	match list with
		| [] -> 0
		| h::t -> h + ft_sum_list t;;

Printf.printf "%d | %d\n"
	(ft_sum_list [10; 20; 30]) (ft_sum_list [0; 20; 30]);;

(** [ft_sum_list list acc]

	A tail recursive function, 
	a function that doesn't build up growing intermediate expressions
*)
let	rec	ft_sum_list_acc list acc =
	match list with
		| [] -> acc
		| h::t -> ft_sum_list_acc t (h + acc);;

Printf.printf "%d | %d\n"
	(ft_sum_list_acc [10; 20; 30] 0) (ft_sum_list_acc [0; 20; 30] 0);;
(**
	[ft_odd_elemts l]

	returns the elements that are in the ODD INDEX
	INDEX 1, 3, 5, 7,...

*)
(* let	rec ft_odd_elements l =
	match l with
		| h::_::t -> h :: ft_odd_elements t
		| _ -> l;; *)

(* let	rec	ft_append_list lx ly =
	match (lx, ly) with
		| lxh::lxt, _ -> lxh :: ft_append_list lxt ly
		| [], lyh::lyt -> lyh :: ft_append_list lx lyt
		| [], [] -> [];;
	
ft_append_list [1; 2; 3; 4] [1; 2];;
ft_append_list [] [];;
ft_append_list [1; 2; 3; 4] [];;
ft_append_list [] [1];; *)

(* let	rec append_list lx ly =
	match lx with
		| [] -> ly
		| h::t -> h :: append_list t ly;; *)

(** [ft_rev l]

	[1; 2; 3]

	[2; 3] :: 1

	[3] :: 2 :: 1

	[] :: 3 :: 2 :: 1

	[] :: 3 :: 2 :: 1

	[1; 2; 3]

	h::t = 1 :: [2; 3]

	h::t = [2; 3] :: 1 :: []

	2 :: 1 :: []

	1 :: [2; 3]
	1 :: 2 :: [3]
	1 :: 2 :: 3 :: [];;

  
	

*)
let	rec	ft_rev l =
	match l with
		| h::t -> (ft_rev t) @ [h]
		| [] -> [];;
		

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
