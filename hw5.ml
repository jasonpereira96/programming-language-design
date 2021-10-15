open List

(* syntax *)
type ident = string

type typ = IntTy | ClassTy of ident
type exp = Num of int | Add of exp * exp | Mul of exp * exp | Var of ident
         | GetField of exp * ident

type cmd = Assign of ident * exp | Seq of cmd * cmd | Skip
         | New of ident * ident * exp list
         | Invoke of ident * exp * ident * exp list | Return of exp


type mdecl = { ret : typ; mname : ident; params : (typ * ident) list; body : cmd }

type cdecl = { cname : ident; super : ident; fields : (typ * ident) list; methods : mdecl list }
(* class cname extends super *)

(* class tables and contexts *)
type class_table = ident -> cdecl option
let lookup_class (ct : class_table) (c : ident) : cdecl option = ct c
                 
type context = ident -> typ option
let empty_context = fun x -> None
let lookup (gamma : context) (x : ident) : typ option = gamma x

(* field and method lookup *)
let rec fields (ct : class_table) (c : ident) : (typ * ident) list =
  if c = "Object" then [] else
    match lookup_class ct c with
    | Some cd -> fields ct cd.super @ cd.fields
    | _ -> []

let types_of_params (params : (typ * ident) list) : typ list =
  List.map fst params

let field_type_aux (l : (typ * ident) list) (f : ident) : typ option =
  match List.find_opt (fun (_, n) -> n = f) l with
  | Some (t, _) -> Some t
  | _ -> None

let field_type (ct : class_table) (c : ident) (f : ident) : typ option =
  field_type_aux (rev (fields ct c)) f

let rec methods (ct : class_table) (c : ident) : mdecl list =
  if c = "Object" then [] else
    match lookup_class ct c with
    | Some cd -> methods ct cd.super @ cd.methods
    | _ -> []

let lookup_method_aux (l : mdecl list) (m : ident) : mdecl option =
  find_opt (fun d -> d.mname = m) l

let lookup_method (ct : class_table) (c : ident) (m : ident) : mdecl option =
  lookup_method_aux (rev (methods ct c)) m

let rec subtype (ct : class_table) (t1 : typ) (t2 : typ) : bool = (match t1, t2 with
  | IntTy, ClassTy c -> false
  | IntTy, IntTy  -> false
  | ClassTy c, IntTy  ->  false
  | ClassTy sub, ClassTy super -> if super = "Object" then true else (if super = sub then true else (subtype ct t1 (ClassTy (match lookup_class ct super with 
    | Some (cdecl) -> cdecl
  ).super)))
)
  

let rec type_of (ct : class_table) (gamma : context) (e : exp) : typ option =
  match e with
  | Num i -> Some IntTy
  | Add (e1, e2) | Mul (e1, e2) ->
      (match type_of ct gamma e1, type_of ct gamma e2 with
       | Some IntTy, Some IntTy -> Some IntTy
       | _, _ -> None)
  | Var x -> lookup gamma x

let rec typecheck_list (ct : class_table) (gamma : context) (es : exp list) (ts : typ list) : bool =
  match es, ts with
  | [], [] -> true
  | e :: erest, t :: trest ->
      (match type_of ct gamma e with
       | Some t1 -> subtype ct t1 t && typecheck_list ct gamma erest trest
       | None -> false)
  | _, _ -> false

let rec typecheck_cmd (ct : class_table) (gamma : context) (c : cmd) : bool =
  match c with
  | Assign (i, e) ->
    (match gamma i, type_of ct gamma e with
     | Some t1, Some t2 -> subtype ct t2 t1
     | _, _ -> false)
  | Seq (c1, c2) -> typecheck_cmd ct gamma c1 && typecheck_cmd ct gamma c2
  | Skip -> true
  | Return e ->
      (match gamma "__ret", type_of ct gamma e with
       | Some t1, Some t2 -> subtype ct t2 t1
       | _, _ -> false)

(* test cases *)  
let ct0 d = if d = "Shape" then
  Some {cname = "Shape"; super = "Object"; fields = [(IntTy, "id")];
        methods = [{ret = IntTy; mname = "area"; params = []; body = Return (Num 0)}]}
      else if d = "Square" then
  Some {cname = "Square"; super = "Shape"; fields = [(IntTy, "side")];
        methods = [{ret = IntTy; mname = "area"; params = [];
                    body = Seq (Assign ("x", GetField (Var "this", "side")),
                       Return (Add (Var "x", Var "x")))}]}
      else None;;

let gamma0 : context = fun y ->
  if y = "s" then Some (ClassTy "Square")
  else if y = "x" || y = "y" then Some IntTy else None;;

let gamma1 : context = fun y ->
  if y = "s" then Some (ClassTy "Shape")
  else if y = "x" || y = "y" then Some IntTy else None;;

let gamma2 : context = fun y ->
  if y = "s1" then Some (ClassTy "Shape")
  else if y = "s2" then Some (ClassTy "Square")
  else if y = "x" || y = "y" then Some IntTy else None;;

let exp2 : exp = GetField (Var "s", "id");;
  
let cmd3 : cmd =
  Seq (New ("s", "Square", [Num 0; Num 2]),
       (* s = new Square(0, 2); *)
       Assign ("y", Add (GetField (Var "s", "side"), Num 1)));;
       (* y = s.side + 1; *)
  
(* for the grad student problem *)
let cmd4 : cmd =
  Seq (New ("s", "Shape", [Num 2]),
       (* s = new Shape(2); *)
       Invoke ("x", Var "s", "area", []));;
       (* x = s.area(); *)
  
let cmd5 : cmd =
  Seq (New ("s", "Square", [Num 0; Num 2]),
       (* s = new Square(0, 2); *)
  Seq (Assign ("y", Add (GetField (Var "s", "side"), Num 1)),
       (* y = s.side + 1; *)
       Invoke ("x", Var "s", "area", [])));;
       (* x = s.area(); *)
  
(* run the tests *)



print_string "Running test 1";; 
let test1 = subtype ct0 (ClassTy "Square") (ClassTy "Object") (* should return true *)

(* let test2 = (type_of ct0 gamma0 exp2 = Some IntTy) should return true *)
  
(* let test3 = typecheck_cmd ct0 gamma0 cmd3 should return true *)
  
(* let test4 = typecheck_cmd ct0 gamma1 cmd4 should return true *)
  
(* let test5 = typecheck_cmd ct0 gamma0 cmd5 should return true *)
