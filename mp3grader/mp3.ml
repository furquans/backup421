open Mp3common
open List

(* Problem 1 *)
let rec even_count l =
match l with
[] -> 0
| (x::xs) -> (even_count xs) + if (x mod 2 = 0) then 1 else 0;;

(* Problem 2 *)
let rec split_sum l f =
match l with
[] -> (0,0)
| (x::xs) -> let (true_n,false_n)=split_sum xs f in
if (f x) then (true_n+x,false_n) else (true_n,false_n+x);;

(* Problem 3 *)
let rec merge l1 l2 =
match l1 with
[] -> l2
| (x::xs) -> (match l2 with
[] -> l1
| (y::ys) -> (x::y::merge xs ys));;

(* Problem 4 *)
let rec range_count l m n =
let rec range_r l m n count =
match l with
[] -> count
| (x::xs) -> if (x > m && x < n) then range_r xs m n (count+1)
else range_r xs m n count
in
range_r l m n 0;;

(* Problem 5 *)
let rec max_index l =
match l with
[] -> []
| (x::xs) -> let rec index_r l max curr ind =
(match l with
[] -> ind
| (y::ys) -> if (y > max) then index_r ys y (curr+1) [curr]
else if (y = max) then index_r ys max (curr+1) (curr::ind)
else index_r ys max (curr+1) ind)
in
index_r xs x 1 [0];;

(* Problem 6 *)
let rec unique_to_neighbor l =
match l with
[] -> []
| (x::xs) -> let rec unique_r l new_list last =
(match l with
[] -> new_list
| (y::ys) -> if (y = last) then unique_r ys new_list last
else unique_r ys (y::new_list) y)
in
let rec rev_list old_l new_l =
(match old_l with
[] -> new_l
| (y::ys) -> rev_list ys (y::new_l))
in
rev_list (unique_r xs [x] x) [];;

(* Problem 7 *)
let remove_if_base = [];;
let remove_if_rec p x r =
if p x then r else (x::r);;

(* Problem 8 *)
let split_sum_base = (0,0);;
let split_sum_rec f x r =
let (true_n,false_n) = r in
if f x then (true_n+x,false_n) else (true_n,false_n+x);;

(* Problem 9 *)
let all_positive_base = true;;
let all_positive_rec r x =
if (r && x>0) then true else false;;

(* Problem 10 *)
let range_count_base = 0;;
let range_count_rec m n r x =
if (x > m && x < n) then (r+1) else r;;

(* Problem 11 *)
let app_all_with fs b l  =
List.map (fun f -> List.map (fun arg2 -> f b arg2) l) fs;;

(* Problem 12 *)
let rev l =
List.fold_left (fun acc y -> (y::acc)) [] l;;
let rle l =
match l with
[] -> []
| (x::xs) -> rev (List.fold_left (fun acc y ->
match acc with
[] -> raise(Failure "Should never occur")
| ((a,b)::zs) -> if (y = a) then ((a,(b+1))::zs)
else ((y,1)::acc)) [(x,1)] xs);;

