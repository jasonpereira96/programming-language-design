(*
problem 1
  (Î»x. x) y evaluates to y
  (Î»x. (Î»y. x)) z evaluates to Î»y.z (lambda y.z)
  (Î»x. (Î»x. x) x) (Î»y. y) evaluates to Î»y.y (lambda y.y)
*)

type ident = string

type exp = Var of ident | Lam of ident * exp | App of exp * exp

(* problem 2 *)
let lam1 = Lam ("x", Lam ("y", App (Var "x", Var "y")))
(* uncomment this *)
let omega : exp = App (Lam ("x", App (Var "x", Var "x")), Lam ("x", App (Var "x", Var "x")))


(* problem 3 *)
let rec vars (l : exp) : ident list = (match l with 
  | Var (ident) -> [ident]
  | Lam (ident, exp) -> ident :: vars exp
  | App (exp1, exp2) -> vars exp1 @ vars exp2
  )

let rec fresh_aux (l : ident list) (i : int): ident =
  let s = String.make 1 (Char.chr i) in
  match List.find_opt (fun t -> t = s) l with
  | Some _ -> let i' = i + 1 in let i'' = if i' > 122 then 97 else i' in fresh_aux l i''
  | None -> s

let fresh (l : exp) : ident = fresh_aux (vars l) 121

let rec subst (x : ident) (l2 : exp) (l : exp) : exp =
  match l with
  | Var y -> if y = x then l2 else Var y
  | App (la, lb) -> App (subst x l2 la, subst x l2 lb)
  | Lam (y, b) -> if y = x then Lam (y, b) else
      let z = fresh l2 in
      Lam (z, subst x l2 (subst y (Var z) b))

let rec eval (l : exp) : exp option =
  match l with
  | App (la, lb) -> (match eval la with
                     | Some (Lam (x, b)) -> Some (subst x lb b)
                     | _ -> None)
  | _ -> Some l

let test3 = eval (App (lam1, Var "y"));;

(* problem 4 
*)

print_string "Problem 1(a)";;
eval (App (Lam ("x", Var "x"), Var "y"));;

print_string "Problem 1(b)";;
eval (App (Lam ("x", Lam ("y", Var "x")), Var "z"));;

print_string "Problem 1(c)";;
eval (App (App (Lam ("x", Lam ("x", Var "x")), Var "x"), Lam ("y", Var "y")));;

(* Did not notice any differences between expected and computed values *)

(* problem 5 *
The function eval() is using call by name because the subst() function does not compute an expression 
fully to a value before applying a function.
We can change this by evaluating a the given expression (l2) to a value before executing the application step.
*)