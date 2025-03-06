(** [factorial int: a] Returns the factorial of input, Returns 0 if input < 0 *)
let	rec	factorial a =
	match a with
		1 -> 1
	| n when n < 0 -> 0
	| _ -> a * factorial(a -1);;

Printf.printf "Factorial 5 = %d, Factorial -42 = %d\n" (factorial (5)) (factorial (-42));;

(** [is_vowel char: c] Checks if input is a vowel *)
let	is_vowel c =
	match c with
		'a' | 'A' | 'e' | 'E' | 'i' | 'I' | 'o' | 'O' | 'u' | 'U' -> true
		| _ -> false;;

Printf.printf "is_vowel a %s, is_vowel b %s, is_vowel A %s, isvowel U %s\n"
	(string_of_bool (is_vowel ('a'))) (string_of_bool (is_vowel ('b'))) (string_of_bool (is_vowel ('A'))) (string_of_bool (is_vowel ('U')));;

let	rec gcd x y =
	let	(px, py) = (abs x, abs y) in
	let	(smaller_num, bigger_num) = (min px py, max px py) in
	match (smaller_num, bigger_num) with
		(0, _) -> bigger_num
		| _ -> gcd (smaller_num) (bigger_num mod smaller_num);;

Printf.printf "GCD 15 10 = %d, -5 -10 = %d, -20 -10 = %d\n"
	(gcd 15 10) (gcd (-5) (-10)) (gcd (-20) (-10));;

let	ft_not x =
	match x with
		_ -> !x;