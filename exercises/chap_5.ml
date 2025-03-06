(*
	Time Complexity Big-O Notation

	Constant Time - O(1)
		arr[2]
		arr.pop()
		arr.shift()

		Done in ONE iteration

	Linear Time - O(n)
		-   for loops based on array's length
		-   finding an element by going through the array one by one
		
		Takes time equal to n

	Quadratic Time - O(n^2) / O(n * n) / O(n * m)
		-   2 Dimensional Loops

		for x, list.length
			for y, list.length
				x + y
		
		O(n * m)
			- Table of 2 * 3
		
		O(n^z)
			- Looping through Z array's of same length

	Logarithmic Time - O(log n)
		-   Binary search on a SORTED LIST
		-   Basically how many times can n / 2 = the iteration it takes

		log 4 = 2, 2 iterations
		find 3 [1, 2, 3, 4] | [1, 2] [3, 4] | [3]

	Linearithmic Time - O(n log n)
		- Loop through array and 
		- Merge Sort

		Merge Sort
		
		Split	[3, 1, 6, 2, 4, 8, 10, 9]
		1.	Split	[3:: ] [1:: ] [6, 2, 4, 8, 10, 9]
		2.	Split	([3::6] [1::2]) [4, 8, 10 ,9]
		3.	Split	([3::6::4] [1::2::8]) [10 ,9]
		4.	Split	merge ((merge_sort 3::6::4::10:[]) (merge_sort 1::2::8::9::[]))
		5.	Split	merge (merge([3, 6, 4, 10]) merge([1, 2, 8, 9]))
		6.	Split	merge (merge([3:: ], [6:: ] [4, 10]) merge([1::] [2::] [8, 9]))
		7.	Split	merge (([3::4], [6::10] [4, 10]) ([1::] [2::] [8, 9]))
*)


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

let rec length lst =
	let rec helper acc = function
		| [] -> acc
		| _::t -> helper (acc + 1) t 
	in helper 0 lst;;


(**
	sort

	sorts the list using insertion sort
	O(n^2);
*)

let rec sort l =
	let rec insert x = function
		| [] -> [x]
		| h::t -> if x <= h then x :: h :: t else h :: insert x t in
	match l with
		| [] -> []
		| h::t -> insert h (sort t);;
;;

let	rec	merge x y =
	match x, y with
		| [], l -> l
		| l, [] -> l
		| xh::xt, yh::yt ->
			if xh < yh
				then xh :: merge xt (yh :: yt)
				else yh :: merge yt (xh :: xt);;

(** 
	[merge_sort l]
*)

let rec merge_sort l =
	match l with
		| [] -> []
		| [a] -> [a]
		|  l -> (
			let length_of_list = length l in
			let middle = length_of_list / 2 in
			merge (merge_sort (take middle l)) (merge_sort (drop middle l))
		);;

(*
	1.	[1; 2; 3; 4]
	2.	insert_rev 1 insert_sort_rev [2; 3; 4]
	3.	insert_rev 1 (insert_rev 2 insert_sort_rev [3; 4])
	4.	insert_rev 1 (insert_rev 2 (insert_rev 2 insert_sort_rev [3; 4]))
	5.	insert_rev 1 (insert_rev 2 (insert_rev 2 insert_rev 3 insert_sort_rev[4]))
	6.	insert_rev 1 (insert_rev 2 (insert_rev 2 insert_rev 3 [4]))
	7.	insert_rev 1 (insert_rev 2 (insert_rev 2 insert_rev 3 [4]))
	8.	insert_rev 1 (insert_rev 2 (insert_rev 2 ([4] @ insert_rev 3 []))
	9.	insert_rev 1 (insert_rev 2 (insert_rev 2 ([4] @ [3]))
	10.	insert_rev 1 (insert_rev 2 (insert_rev 2 ([4; 3]))
	11.	insert_rev 1 (insert_rev 2 ([4] @ insert_rev 2 [3]))
	12.	insert_rev 1 (insert_rev 2 ([4] @ [3] insert_rev 2 []))
	13.	insert_rev 1 (insert_rev 2 ([4] @ [3] @ [2]))
	14.	insert_rev 1 (insert_rev 2 [4; 3; 2])
	15.	insert_rev 1 ([4] @ insert_rev 2 [3; 2])
	16.	insert_rev 1 ([4] @ [3] insert_rev 2 [2])
	17.	insert_rev 1 ([4] @ [3] @ [2] insert_rev 2 [])
	18.	insert_rev 1 ([4] @ [3] @ [2] @ [2])
	19.	insert_rev 1 [4; 3; 2]
	20.	insert_rev 1 [4; 3; 2]
	21. [4] @ insert_rev 1 [3; 2]
	22. [4] @ [3] @ insert_rev 1 [2]
	23. [4] @ [3] @ [2] insert_rev 1 []
	24.	[4] @ [3] @ [2] @ [1]
	25. [4; 3; 2; 1]
*)
(**
	[insertion_sort_reverse l]

insrt_rev
1.	Always make sure the tail is sorted before proceeding
2.	[] -> [prev_v]
3.	prev_v <= h then
	1.	[h] @ insrt_rev prev_v t
4.	else
	1.	[prev_v] @ insrt_rev h t 
*)
let rec insert_sort_rev l =
	let rec insert_rev y = function
		| [] -> [y]
		| h::t -> if (y <= h) then [h] @ insert_rev y t  else [y] @ [h] @ t in
	match l with
		| [] -> []
		| h::t -> insert_rev h (insert_sort_rev t);;

(**
	[is_sorted]

	[5; 4; 3; 2; 1];	(* Reverse sorted *)
	[1; 2; 3; 4; 5];	(* Already sorted *)
	[3; 1; 4; 1; 5];	(* Dupes *)


	sorted_desc
	sorted_asc

	1.	if first comparison is asc/desc
	2.	then compare all other numbers using sorted_asc/desc

	1.	3 1 4 1 5
	2.	3::1::t
	3.	3 > 1 then sorted_desc 1 t else sorted_asc 1 t
*)
let	rec	is_sorted l = 
	let	rec	sorted_asc y = function
		| [] -> true
		| h::t -> if (y <= h) then sorted_asc h t else false in
	let	rec sorted_desc y = function
		| [] -> true
		| h::t -> if (y >= h) then sorted_desc h t else false in
	match l with
		| [] -> true
		| _::[] -> true
		| h::m::t -> 
			if (h = m) then is_sorted ([m] @ t)
			else if (h > m) then sorted_desc m t
			else sorted_asc m t;; 