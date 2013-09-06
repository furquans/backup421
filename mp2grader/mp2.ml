(* CS421 - Fall 2013
 * MP2 
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You will want to change how a number of these start.
 *)


(*Problem 1*)
let sqr x = x *. x;;
let dist (x1, y1) (x2, y2) = sqrt (sqr (x2-.x1) +. sqr (y2-.y1));;

(*Problem 2*)
let rec fibo_num n =
match n with
n when n <= 0 -> 0
| 1 -> 1
| n -> fibo_num (n-1) + fibo_num(n-2);;

(*Problem 3*)
let rec fibo_sum n =
let x = fibo_num n in
match x with
0 -> 0
| x -> x + fibo_sum (n-1);;

(*Problem 4*)
let reverse_triple_to_list (a, b, c) = [c; b; a];;

(*Problem 5*)
let rec sum l =
match l with
[] -> 0
| (x::xs) -> x + sum xs;;

(*Problem 6*)
let rec minr l default =
match l with
[] -> default
| (x::xs) -> if (x < default) then minr xs x
else minr xs default;;

let rec min l default =
match l with
[] -> default
| (x::xs) -> minr xs x;;

(*Problem 7*)
let rec is_sorted_ascend l =
match l with
[] -> true
| (x::[]) -> true
| (x::y::xs) -> if (x <= y) then is_sorted_ascend (y::xs)
else false;;

(*Problem 8*)
let rec zip l1 l2 =
match l1 with
[] -> []
| (x::xs) -> (match l2 with
[] -> []
| (y::ys) -> ((x,y)::zip xs ys));;

(*Problem 9*)
let rec unzip l =
match l with
[] -> [], []
| ((x1,x2)::xs) -> let l1,l2 = unzip xs in (x1::l1), (x2::l2);;

(*Problem 10*)
let rec add_odd_pos l =
match l with
[] -> 0
| (x1::[]) -> x1
| (x1::x2::xs) -> x1 + add_odd_pos xs;;

(*Problem 11*)
let rec insert n l =
match l with
[] -> (n::[])
| (x::xs) -> if (x <= n) then (x::insert n xs)
else (n::x::xs);;

(*Extra Credit *)
(*Problem 12*)
let rec check_div n d =
match d with
1 -> true
| d -> if (n mod d == 0) then false else check_div n (d-1);;

let is_prime n = check_div n (n-1);;

let primes n =
let rec get_primes num count n list =
if (count >= n) then list
else
match is_prime num with
true -> get_primes (num+1) (count+1) n (num::list)
| false -> get_primes (num+1) count n list
in
get_primes 2 0 n [];;