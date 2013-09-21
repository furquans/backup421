open Mp4common

(* Problem 1 *)
let addk n m k = k (n+m);;
let subk n m k = k (n-m);;
let mulk n m k = k (n*m);;
let posk x k = k (x>0);;
let float_addk a b k = k (a+.b);;
let float_divk a b k = k (a/.b);;
let catk str1 str2 k = k (str1^str2);;
let consk e l k = k (e::l);;
let eqk x y k = k (x=y);;
let geqk x y k = k (x>=y);;

(* Problem 2 *)
let poly x k =
mulk x x
(fun r1 -> mulk x r1
(fun r2 -> addk x 1
(fun r3 -> addk r2 r3 k)));;

(* Problem 3 *)
let composek f g x k = 
f x (fun r1 -> g r1 (fun r2 -> k r2));;

(* Problem 4 *)

let rec inverse_square_series n = 
if (n<=0) then 0.0
else 1.0/.(float_of_int (n*n)) +. inverse_square_series (n-1);;

let leqk a b k = k (a<=b);;
let float_of_intk x k = k (float_of_int x);;

let rec inverse_square_seriesk n k = 
leqk n 0
(fun b -> if b then k 0.0
else inverse_square_seriesk (n-1)
(fun r1 -> mulk n n
(fun m -> float_of_intk m
(fun f -> float_divk 1.0 f
(fun r2 -> float_addk r2 r1 k)))));;

(* Problem 5 *)
let rec rev_map f l =
match l with
[] -> []
| (x::xs) -> let r1 = rev_map f xs in (f x::r1);;

let rec rev_mapk f l k = 
match l with
[] -> k []
| (x::xs) -> rev_mapk f xs
(fun r1 -> f x
(fun r2 -> consk r2 r1 k));;

(* Problem 6 *)
let rec partition l p = 
match l with
[] -> [], []
| (x::xs) -> let (r1,r2) = partition xs p in
if p x then (x::r1),r2 else r1,(x::r2);;

let rec partitionk l p k =  
match l with
[] -> k ([],[])
| (x::xs) -> partitionk xs p
(fun (r1,r2) -> p x
(fun b -> if b then
consk x r1 (fun l1 -> k (l1,r2))
else consk x r2 (fun l2 -> k (r1,l2))));;

(* Problem 7 *)
let rec findk l p normalk exceptionk = 
match l with
[] -> exceptionk ()
| (x::xs) -> p x
(fun b -> if b then normalk x
else findk xs p normalk exceptionk);;

(* Problem 8 *)
let rec appk l x k =
match l with
[] -> k x
| (f::fs) -> appk fs x (fun r1 -> f r1 k);;


