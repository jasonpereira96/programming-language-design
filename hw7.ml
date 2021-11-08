open List

type ident = string

type exp = Var of ident | Fun of ident * exp | App of exp * exp
           | Int of int | Bool of bool | Add of exp * exp | Eq of exp * exp
           | If of exp * exp * exp
           | Inl of exp | Inr of exp
           | Match of exp * ident * exp * ident * exp

type env = ident -> value option
 and value = IntVal of int | BoolVal of bool | Closure of ident * exp * env
           | InlVal of value | InrVal of value

let empty_env : env = fun y -> None
let lookup (r : env) (x : ident) : value option = r x
let update (r : env) (x : ident) (v : value) = fun y -> if y = x then Some v else r y
             
let rec eval (e : exp) (r : env) : value option =
   match e with
   | Var x -> lookup r x
   | Int i -> Some (IntVal i)
   | Bool b -> Some (BoolVal b)
   | Add (e1, e2) ->
       (match eval e1 r, eval e2 r with
        | Some (IntVal i1), Some (IntVal i2) -> Some (IntVal (i1 + i2))
        | _, _ -> None)
   | Eq (e1, e2) ->
       (match eval e1 r, eval e2 r with
        | Some v1, Some v2 -> Some (BoolVal (v1 = v2))
        | _, _ -> None)
   | If (e, e1, e2) ->
       (match eval e r with
        | Some (BoolVal b) -> eval (if b then e1 else e2) r
        | _ -> None)
   | Fun (ident, exp1) -> Some (Closure (ident, exp1, r))
   | App (e1, e2) -> (match (eval e1 r), (eval e2 r) with
        | Some(Closure(x, e, r1)) , Some(v) -> (eval e (update r1 x v))
        | _, _ -> None)
   | Inl (e1) -> (match eval e1 r with
      | Some (v) -> Some(InlVal(v)))
   | Inr (e1) -> (match eval e1 r with
      | Some (v) -> Some(InrVal(v)))


let env1 : env = update empty_env "b" (IntVal 1);;
print_string "Test 1";;
let test1 : value option = eval (App (Fun ("a", Add (Var "a", Var "b")), Int 5)) env1;;

print_string "Test 2";;
let test2 : value option = eval (Inr (Add (Int 3, Int 4))) empty_env;;

print_string "Test 3";;
let test3 : value option = eval (Match (Inr (Bool false), "i", Var "i", "b", If (Var "b", Int 1, Int 0))) empty_env;;


let env4 : env = update (update empty_env "x" (InrVal (IntVal 5)))
  "f" (Closure ("y", Match (Var "y", "a", Var "a", "b", Add (Var "b", Var "x")),
                        update empty_env "x" (IntVal 2)))
let test4 : value option = eval (App (Var "f", Var "x")) env4 (* should return Some (IntVal 7) *)
