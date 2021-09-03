type name = string

type stmt = AssignStmt of name * int | IfStmt of name * stmt * stmt | DoWhileStmt of name * stmt (* add problem 2 here *)

let stmt1 : stmt = IfStmt ("x", AssignStmt ("y", 5), AssignStmt ("z", 2));;

(* answer to problem 1:  *)
(*
if (x) { y = 5; } else { z = 2; }
*)

(* problem 3 *)
let stmt2 : stmt = DoWhileStmt("y", IfStmt ("x", AssignStmt ("x", 0), AssignStmt ("x", 1)));;

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
  | If (e, e1, e2) -> typecheck e BoolTy && typecheck e1 t && typecheck e2 t
  | Eq (e1, e2) -> ((typecheck e1 IntTy && typecheck e2 IntTy) || (typecheck e1 BoolTy && typecheck e2 BoolTy))
      && t = BoolTy
;;

typecheck (If (Bool true, Num 1, Num 3)) IntTy;; (* should return true *)
typecheck (If (Bool true, Bool false, Num 3)) BoolTy;; (* should return false *)

print_string "Test cases for problem 5";;
typecheck (Eq (Num 3, Num 5)) IntTy;; (* should return false *)
typecheck (Eq (Num 3, Num 5)) BoolTy;; (* should return true *)
typecheck (Eq (Num 3, Bool true)) IntTy;; (* should return false *)
typecheck (Eq (Num 3, Bool true)) BoolTy;; (* should return false *)
typecheck (Eq (Bool true, Num 5)) IntTy;; (* should return false *)
typecheck (Eq (Bool false, Num 5)) BoolTy;; (* should return false *)
typecheck (Eq (Bool false, Bool true)) IntTy;; (* should return false *)
typecheck (Eq (Bool false, Bool true)) BoolTy;; (* should return true *)