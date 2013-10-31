type bool_exp =
VarName of string
| BoolConst of bool
| NotBool of bool_exp
| ConBool of bool_exp * bool_exp
| DisBool of bool_exp * bool_exp;;

let rec bool_exp_eval e env =
match e with
VarName(v) -> env v
| BoolConst(b) -> b
| NotBool(b) -> not (bool_exp_eval b env)
| ConBool(b1,b2) -> (bool_exp_eval b1 env) && (bool_exp_eval b2 env)
| DisBool(b1,b2) -> (bool_exp_eval b1 env) || (bool_exp_eval b2 env);;

let exp1 = (DisBool(ConBool(VarName("a"),VarName("b")),NotBool(BoolConst(true))));;

let env =
fun s ->
match s with
"a" -> true
| "b" -> false;;