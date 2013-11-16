open Mp10common;;

let const_to_val c = 
  match c with
      BoolConst b -> BoolVal(b)
    | IntConst i -> IntVal(i)
    | RealConst r -> RealVal(r)
    | StringConst s -> StringVal(s)
    | NilConst -> ListVal([])
    | UnitConst -> UnitVal;;

let monOpApply unop v = 
  match (unop,v) with
      (IntNegOp, IntVal i) -> IntVal(-i)
    | (HdOp, ListVal lst) -> 
      (match lst with [] -> failwith "list is empty"
	| x::xs -> x)
    | (TlOp, ListVal lst) ->
      (match lst with [] -> failwith "list is empty"
	| x::xs -> ListVal(xs) )
    | (FstOp, PairVal(v1,_)) -> v1
    | (SndOp, PairVal(_,v2)) -> v2
    | (PrintStringOp, StringVal s) -> print_string s; UnitVal
    | (_,_) -> failwith "invalid operation"

let binOpApply binop v1 v2 = 
  match (binop,v1,v2) with
      (IntPlusOp, IntVal i1, IntVal i2) -> IntVal (i1+i2)
    | (IntMinusOp, IntVal i1, IntVal i2) -> IntVal (i1-i2)
    | (IntTimesOp, IntVal i1, IntVal i2) -> IntVal (i1*i2)
    | (IntDivOp, IntVal i1, IntVal i2) -> IntVal (i1/i2)
    | (RealPlusOp, RealVal r1, RealVal r2) -> RealVal (r1+.r2)
    | (RealMinusOp, RealVal r1, RealVal r2) -> RealVal (r1-.r2)
    | (RealTimesOp, RealVal r1, RealVal r2) -> RealVal (r1*.r2)
    | (RealDivOp, RealVal r1, RealVal r2) -> RealVal (r1/.r2)
    | (ConcatOp, StringVal s1, StringVal s2) -> StringVal (s1^s2)
    | (ConsOp, h, ListVal t) -> ListVal (h::t)
    | (CommaOp, v1, v2) -> PairVal(v1,v2)
    | (EqOp, v1, v2) -> BoolVal(v1 = v2)
    | (GreaterOp, v1, v2) -> BoolVal(v1>v2)
    | (_,_,_) -> failwith "invalid operation"

let rec eval_exp (exp, m) = 
  match exp with
      ConstExp c -> const_to_val c
    | VarExp x -> (match lookup_env m x with None -> failwith "couldnt find the variable."
	                                   | Some v -> v)
    | FnExp(x,e) -> ClosureVal(x,e,m)
    | AppExp(e1,e2) ->
      (match eval_exp (e1,m) with
	  ClosureVal(x,e',m') -> 
	    (match eval_exp (e2,m) with
		v' -> eval_exp (e',(ins_env m' x v')))
	| _ -> failwith "invalid return of closure")
    | MonOpAppExp(mon,e) -> let v = eval_exp (e,m) in monOpApply mon v
    | BinOpAppExp(b,e1,e2) -> 
      let v1 = eval_exp (e1,m) in
      let v2 = eval_exp (e2,m) in
      binOpApply b v1 v2
    | IfExp(e1,e2,e3) ->
      (match eval_exp (e1,m) with
	  BoolVal(true) -> eval_exp (e2,m)
	| BoolVal(false) -> eval_exp (e3,m)
	| _ -> failwith "invalid return value for if")
    | LetExp(d,e) ->
      let (b,m') = eval_dec (d,m) in
      eval_exp (e,(sum_env m' m))
    | _ -> failwith "eval_exp is not implemented yet."

and eval_dec (dec, m) = 
  match dec with
      Val(x,e) -> let v = eval_exp (e,m) in 
		  if x = "" then ([(None,v)],empty_env) else ([(Some x,v)],(make_env x v))
    | _ -> failwith "eval_dec is not implemented yet."
