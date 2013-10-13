open Mp6common

let rec gather_exp_ty_substitution gamma exp tau =
    let judgment = ExpJudgment(gamma, exp, tau) in
    match exp
    with ConstExp c ->
         let tau' = const_signature c in
         (match unify [(tau, freshInstance tau')]
          with None       -> None
             | Some sigma -> Some(Proof([],judgment), sigma))
      | VarExp v ->
	(match Mp6common.lookup_env gamma v with
	    None -> None
	  | Some tau' -> (match unify [(tau, freshInstance tau')]
	    with None -> None
	      | Some sigma -> Some(Proof([],judgment), sigma)))
      | MonOpAppExp (m,e1) ->
	let tau1 = fresh () in
	(match gather_exp_ty_substitution gamma e1 tau1 with
	    None -> None
	  | Some (p1,s1) -> let tau' = monop_signature m in
			    (match unify [(monoTy_lift_subst s1 (mk_fun_ty tau1 tau), freshInstance tau')] with
				None -> None
			      | Some sigma -> Some(Proof([p1],judgment), subst_compose sigma s1)))
      | BinOpAppExp (b,e1,e2) ->
	let tau1 = fresh () in
	(match gather_exp_ty_substitution gamma e1 tau1 with
	    None -> None
	  | Some (p1,s1) -> let tau2 = fresh () in
			    (match gather_exp_ty_substitution (env_lift_subst s1 gamma) e2 tau2 with
				None -> None
			      | Some (p2,s2) -> let tau' = binop_signature b in
						let s21 = subst_compose s2 s1 in
						(match unify [(monoTy_lift_subst s21 (mk_fun_ty tau1 (mk_fun_ty tau2 tau)), freshInstance tau')] with
						    None -> None
						  | Some sigma -> Some(Proof([p1]@[p2],judgment), subst_compose sigma s21))))
      | IfExp (e1,e2,e3) ->
	(match gather_exp_ty_substitution gamma e1 bool_ty with
	    None -> None
	  | Some (p1,s1) -> (match gather_exp_ty_substitution (env_lift_subst s1 gamma) e2 (monoTy_lift_subst s1 tau) with
	      None -> None
	      | Some (p2,s2) -> let s21 = subst_compose s2 s1 in
				(match gather_exp_ty_substitution (env_lift_subst s21 gamma) e3 (monoTy_lift_subst s21 tau) with
				    None -> None
				  | Some (p3,s3) -> Some(Proof([p1]@[p2]@[p3],judgment), subst_compose s3 s21))))
      | FnExp (x,e) ->
	let tau1 = fresh () in
	let tau2 = fresh () in
	(match gather_exp_ty_substitution (ins_env gamma x (polyTy_of_monoTy tau1)) e tau2 with
	    None -> None
	  | Some (p,s) -> (match unify [(monoTy_lift_subst s tau,monoTy_lift_subst s (mk_fun_ty tau1 tau2))] with
	      None -> None
	      | Some sigma -> Some(Proof([p],judgment), subst_compose sigma s)))
      | AppExp (e1,e2) ->
	let tau1 = fresh () in
	(match gather_exp_ty_substitution gamma e1 (mk_fun_ty tau1 tau) with
	    None -> None
	  | Some(p1,s1) -> (match gather_exp_ty_substitution (env_lift_subst s1 gamma) e2 (monoTy_lift_subst s1 tau1) with
	      None -> None
	      | Some(p2,s2) -> Some(Proof([p1]@[p2],judgment), subst_compose s2 s1)))
      | RaiseExp (e) ->
	(match gather_exp_ty_substitution gamma e int_ty with
	    None -> None
	  | Some(p,s) -> Some(Proof([p],judgment), s))
      | HandleExp (e,_,e1,nelist) ->
	(match gather_exp_ty_substitution gamma e tau with
	    None -> None
	  | Some(p,s) -> (match gather_exp_ty_substitution (env_lift_subst s gamma) e1 (monoTy_lift_subst s tau) with
	      None -> None
	      | Some(p1,s1) -> let rec gather_list exp pn sn =
				 (match exp with
				     [] -> Some(pn,sn)
				   | (_,e)::nes -> (match gather_exp_ty_substitution (env_lift_subst sn gamma) e (monoTy_lift_subst sn tau) with
				       None -> None
				       | Some(pl,sl) -> gather_list nes (pn@[pl]) (subst_compose sl sn))) in
			       (match gather_list nelist ([p]@[p1]) (subst_compose s1 s) with
				   None -> None
				 | Some(pn,sn) -> Some(Proof(pn,judgment),sn))))
      | LetExp(d,e) -> (match gather_dec_ty_substitution gamma d with
	  None -> None
	  | Some(pd,delta,sd) -> (match gather_exp_ty_substitution (sum_env delta (env_lift_subst sd gamma)) e (monoTy_lift_subst sd tau) with
	      None -> None
	      | Some(pe,se) -> Some(Proof([pd]@[pe],judgment),subst_compose se sd)))

and gather_dec_ty_substitution gamma dec = 
  match dec with
      Val(x,e) -> let tau = fresh () in
		  (match gather_exp_ty_substitution gamma e tau with
		      None -> None
		    | Some (p,s) -> let env = make_env x (gen (env_lift_subst s gamma) (monoTy_lift_subst s tau)) in
				    Some(Proof([p],DecJudgment(gamma,dec,env)), env, s))
    | Rec(f,x,e) -> let tau1 = fresh () in
		    let tau2 = fresh () in
		    let tau12 = mk_fun_ty tau1 tau2 in
		    let gamma' = ins_env gamma x (polyTy_of_monoTy tau1) in
		    let gamma' = ins_env gamma' f (polyTy_of_monoTy tau12) in
		    (match gather_exp_ty_substitution gamma' e tau2 with
			None -> None
		      | Some(p,s) -> let env = make_env f (gen (env_lift_subst s gamma) (monoTy_lift_subst s tau12)) in
				     Some(Proof([p],DecJudgment(gamma,dec,env)),env,s))
    | Seq(d1,d2) -> (match gather_dec_ty_substitution gamma d1 with
	None -> None
	| Some(p1,delta1,s1) -> (match gather_dec_ty_substitution (env_lift_subst s1 (sum_env delta1 gamma)) d2 with
	    None -> None
	    | Some(p2,delta2,s2) -> let s21 = subst_compose s2 s1 in
				    let env = env_lift_subst s21 (sum_env delta2 delta1) in
				    Some(Proof([p1]@[p2],DecJudgment(gamma,dec,env)),env,s21)))
    | Local(d1,d2) -> (match gather_dec_ty_substitution gamma d1 with
	None -> None
	| Some(p1,delta1,s1) -> (match gather_dec_ty_substitution (env_lift_subst s1 (sum_env delta1 gamma)) d2 with
	    None -> None
	    | Some(p2,delta2,s2) -> let s21 = subst_compose s2 s1 in
				    let env = env_lift_subst s21 delta2 in
				    Some(Proof([p1]@[p2],DecJudgment(gamma,dec,env)),env,s21)))
