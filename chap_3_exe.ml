let	ft_not x =
  match x with
    | true -> false
	| false -> true;;

let	rec	ft_sum_up_to n =
	match n with
		| n when n <= 0 -> 0
		| _ -> n + ft_sum_up_to (n - 1);;

let	ft_pow x n =
	let x = abs x in
	let n = abs n in
	match (x, n) with
		| 0, _ -> 1
		| _, 0 -> 1
		| x, n -> int_of_float ((float_of_int x) ** (float_of_int n));;

let	_ =
	match 1 + 1 with
		| 2 -> (match 2 + 2 with
			| 3 -> 4
			| 4 -> 5
			| _ -> 0)
		| _ -> 0;;
(* evaluates to 5 *)

let	ft_is_upper c =
	match c with
		| c when c >= 'A' && c <= 'Z' -> true
		| _ -> false;;

let	ft_is_lower c =
match c with
	| c when c >= 'a' && c <= 'z' -> true
	| _ -> false;;


Printf.printf "ft_not false = %s\n" (string_of_bool (ft_not (false)));;
Printf.printf "ft_sum_up_to 10 = %d\n" (ft_sum_up_to 10);;
Printf.printf "ft_pow 2^4 = %d\n" (ft_pow 2 4);;
Printf.printf "is_upper 1 = %s, A = %s, Z = %s\n"
	(string_of_bool (ft_is_upper '1')) (string_of_bool (ft_is_upper 'A'))
	(string_of_bool (ft_is_upper 'Z'));;
Printf.printf "is_lower A = %s, a = %s, z = %s\n"
	(string_of_bool (ft_is_lower 'X')) (string_of_bool (ft_is_lower 'a'))
	(string_of_bool (ft_is_lower 'z'));;

