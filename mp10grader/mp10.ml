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
		v' -> eval_exp (e',(ins_env m' x v'))))
    | MonOpAppExp(mon,e) -> let v = eval_exp (e,m) in monOpApply mon v
    | BinOpAppExp(b,e1,e2) -> 
      let v1 = eval_exp (e1,m) in
      let v2 = eval_exp (e2,m) in
      binOpApply b v1 v2
    | _ -> failwith "eval_exp is not implemented yet."

let eval_dec (dec, m) = 
  match dec with
      Val(x,e) -> let v = eval_exp (e,m) in 
		  if x = "" then ([(None,v)],empty_env) else ([(Some x,v)],(make_env x v))
    | _ -> failwith "eval_dec is not implemented yet."
