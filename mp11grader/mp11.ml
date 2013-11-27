(* File: mp11-skeleton.ml *)

open Mp11common;;

let rec app_cont_to_value env k v =
  match k with
      External -> Final(v)
    | ContVarCPS(i) -> (match lookup_cont env i with
	                  None -> Failed
	                | Some(ck, envlst) -> app_cont_to_value envlst ck v)
    | ContCPS(y,e) -> Intermediate(ValueBinding(y,v)::env,e)
    | _ -> Failed

let rec one_step_exp_cps_eval env exp_cps = raise (Failure "Not implemented yet.")
