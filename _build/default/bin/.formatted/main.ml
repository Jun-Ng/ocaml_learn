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

(*
   let a equals to 3 
		in this expression
			( 
				let b = a * a in this sub-expression
					in a + b
			)
	Since it is defined in local scope of let...in
	You're unable to use variable a globally
*)
let a = 3 in
let b = a * a in
a + b

(* Declaring functions *)
let cube x = x * x * x;;

cube 10

let neg x = x < 0;;

neg (-1);;
neg 10

(* Printf.printf "Func Neg %s %s" (string_of_bool (neg (-10))) (string_of_bool (neg (1)));; *)

let isvowel c = c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u';;

isvowel 'x'

let addToten a b = a + b
