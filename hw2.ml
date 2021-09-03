type name = string

type stmt = AssignStmt of name * int | IfStmt of name * stmt * stmt (* add problem 2 here *)

let stmt1 : stmt = IfStmt ("x", AssignStmt ("y", 5), AssignStmt ("z", 2));;

(* answer to problem 1:  *)

(* problem 3 *)
(* let stmt2 : stmt = *)

type exp = Num of int
  | Add of exp * exp 
  | Bool of bool
  | And of exp * exp 
  | Not of exp
  | Eq of exp * exp
  | If of exp * exp * exp

type typ = IntTy | BoolTy

let rec typecheck (e : exp) (t : typ) : bool =
  match e with
  | Num _ -> t = IntTy
  | Bool _ -> t = BoolTy
  | Add (e1, e2) -> typecheck e1 IntTy && typecheck e2 IntTy && t = IntTy
  | And (e1, e2) -> typecheck e1 BoolTy && typecheck e2 BoolTy && t = BoolTy
  | Not e -> typecheck e BoolTy && t = BoolTy
  (* problems 4 and 5 *)
;;

typecheck (If (Bool true, Num 1, Num 3)) IntTy;; (* should return true *)
typecheck (If (Bool true, Bool false, Num 3)) BoolTy;; (* should return false *)
