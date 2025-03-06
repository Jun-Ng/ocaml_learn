(* defining a name for an expression *)
let x = 50;;

x

(* an expression *)
let outer =
  let x = 10 in
  let y = 20 in
  x + y
;;

outer;;

(* This documents function *)
let a = 3 in
let b = a * a in
a + b

(** [cube x] Calculates cubed value of the input*)
let cube x = x * x * x;;

cube 10

(** [neg x] Checks if input is negative*)
let neg x = x < 0;;
neg (-1);;
neg 10;;

(** [is_vowel c] Checks if input is a vowel *)
let is_vowel c = c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u';;
is_vowel 'x';;

let add_to_ten a b = a + b = 10;;
add_to_ten 6 4;;

(** [factorial a] Returns the factorial input; 0 = 1 *)
let rec factorial a = 
	if (a < 0)
		then 0
	else if (a = 0 || a = 1)
		then 1
	else a * factorial (a - 1);;
factorial 1;;
(* Printf.printf "%d %d %d" (factorial 4) (factorial 0) (factorial (-1));; *)

(** [ft_gcd x y] Returns the gcd of both numbers *)
let	rec	ft_gcd x y =
	let	px = if x < 0 then -x else x in
	let	py = if y < 0 then -y else y in
	let	smaller_num = if px < py then px else py in
	let bigger_num = if px < py then py else px in
	if (smaller_num = 0) then bigger_num else ft_gcd smaller_num (bigger_num mod smaller_num);;
Printf.printf "%d %d %d" (ft_gcd 5 10) (ft_gcd (-10) (-5)) (ft_gcd (-10) 5);;

(** [ft_sum_up_to_num n] Returns the sum of all natural numbers <= n *)
let	ft_sum_up_to_num n = 
	if (n <= 0) then 0
	else (n * (n + 1)) / 2;;

let ft_pow x n = int_of_float((float_of_int x) ** (float_of_int n));;

let	ft_is_consonant c = 
	c != 'a'
	|| c != 'e'
	|| c != 'i'
	|| c != 'o'
	|| c != 'u';;

Printf.printf "\nft_sum_up_to_num %d" (ft_sum_up_to_num (-1));;
Printf.printf "\nft_pow %d" (ft_pow (2) (-2));;
Printf.printf "\nis_consonant %s" (string_of_bool(ft_is_consonant 'a'));;
