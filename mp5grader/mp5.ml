(* File: mp5-skeleton.ml *)

open Mp5common

(* Problem 1 *)
let rec import_list lst =
match lst with
[] -> ConstExp(NilConst)
| ((s,i)::xs) -> BinOpAppExp(ConsOp,
BinOpAppExp(CommaOp, ConstExp(StringConst s), ConstExp(IntConst i)),
import_list xs);;

(* Problem 2 *)
let list_all =
Rec("list_all", "p",
FnExp("xs",
IfExp(BinOpAppExp(EqOp, VarExp("xs"), ConstExp(NilConst)),
ConstExp(BoolConst(true)),
IfExp(AppExp(VarExp("p"),MonOpAppExp(HdOp,VarExp("xs"))),
IfExp(AppExp(AppExp(VarExp("list_all"),VarExp("p")),MonOpAppExp(TlOp,VarExp("xs"))),
ConstExp(BoolConst(true)),
ConstExp(BoolConst(false))),
ConstExp(BoolConst(false))))));;

(* Problem 3 *)
let rec cal_max_exp_height exp =
match exp with
VarExp(_)
| ConstExp(_) -> 1
| MonOpAppExp(_,e)
| FnExp(_,e) -> 1 + (cal_max_exp_height e)
| BinOpAppExp(_,e1,e2)
| AppExp(e1,e2) -> 1 + (max (cal_max_exp_height e1) (cal_max_exp_height e2))
| IfExp(e1,e2,e3) -> 1 + (max (max (cal_max_exp_height e1) (cal_max_exp_height e2)) (cal_max_exp_height e3))
| LetExp(d,e) -> 1 + (max (cal_max_dec_height d) (cal_max_exp_height e))

and cal_max_dec_height dec =
match dec with
Val(_,e)
| Rec(_,_,e) -> 1 + (cal_max_exp_height e)
| Seq(d1,d2) -> 1 + (max (cal_max_dec_height d1) (cal_max_dec_height d2));;

(* Problem 4 *)
let rec freeVarsInExp exp =
match exp with
VarExp(v) -> [v]
| ConstExp(_) -> []
| MonOpAppExp(_,e) -> (freeVarsInExp e)
| BinOpAppExp(_,e1,e2)
| AppExp(e1,e2) -> (freeVarsInExp e1) @ (freeVarsInExp e2)
| IfExp(e1,e2,e3) -> ((freeVarsInExp e1) @ (freeVarsInExp e2)) @ (freeVarsInExp e3)
| FnExp(s,e) -> List.filter (fun v -> not (v=s)) (freeVarsInExp e)
| LetExp(d,e) -> let (fv,bv) = freeAndBindingVarsInDec d
in
(fv @ (List.filter (fun v -> not(List.mem v bv)) (freeVarsInExp e)))

and freeAndBindingVarsInDec dec = 
match dec with
Val(s,e) -> (freeVarsInExp e, [s])
| Rec(s1,s2,e) -> (List.filter (fun v -> not(List.mem v [s1;s2])) (freeVarsInExp e),[s1])
| Seq(d1,d2) -> let (fv1,bv1) = freeAndBindingVarsInDec d1
in
let (fv2,bv2) = freeAndBindingVarsInDec d2
in
(fv1 @ (List.filter (fun v -> not(List.mem v bv1)) fv2), bv1 @ bv2);;

(* Problem 5 *)
let rec cps_exp e k kx =
match e with
VarExp(v) -> (VarCPS(k,v),kx)
| ConstExp(c) -> (ConstCPS(k,c),kx)
| IfExp(e1,e2,e3) -> let (c2,kx2) = cps_exp e2 k kx in
let (c3,kx3) = cps_exp e3 k kx2 in
let v = freshFor ((freeVarsInExp e1) @ (freeVarsInExp e2) @ (freeVarsInExp e3) @ (freeVarsInContCPS k)) in
cps_exp e1 (ContCPS(v,IfCPS(v,c2,c3))) kx3
| MonOpAppExp(m,e) -> let v = freshFor ((freeVarsInExp e) @ (freeVarsInContCPS k)) in
cps_exp e (ContCPS(v,MonOpAppCPS(k,m,v))) kx
| BinOpAppExp(b,e1,e2) -> let v1 = freshFor ((freeVarsInExp e1) @ (freeVarsInExp e2) @ (freeVarsInContCPS k)) in
let v2 = freshFor ((freeVarsInExp e1) @ (freeVarsInExp e2) @ (freeVarsInContCPS k) @ [v1]) in
let (c2,kx2) = cps_exp e2 (ContCPS(v2,BinOpAppCPS(k,b,v1,v2))) kx in
cps_exp e1 (ContCPS(v1,c2)) kx2
| AppExp(e1,e2) -> let v1 = freshFor ((freeVarsInExp e1) @ (freeVarsInContCPS k)) in
let v2 = freshFor ((freeVarsInContCPS k) @ [v1]) in
let (c2,kx2) = cps_exp e2 (ContCPS(v2, AppCPS(k,v1,v2))) kx in
cps_exp e1 (ContCPS(v1,c2)) kx2
| FnExp(s,e) -> let (c1,kx1) = cps_exp e (ContVarCPS kx) (kx+1) in
(FnCPS(k,s,kx,c1),kx1)
| LetExp(d,e) -> let (c1,kx1) = cps_exp e k kx in
cps_dec d c1 kx1

and cps_dec dec ecps kx =
match dec with
Val(s,e) -> cps_exp e (ContCPS(s,ecps)) kx
| Seq(d1,d2) -> let (c2,kx2) = cps_dec d2 ecps kx in
cps_dec d1 c2 kx2
| Rec(s1,s2,e) -> let (c1,kx1) = cps_exp e (ContVarCPS kx) (kx+1) in
(FixCPS(ContCPS(s1,ecps),s1,s2,kx,c1),kx1);;
