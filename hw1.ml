(* question 1 *)
let rec sum (n : int) : int = if n = 0 then 0 else n + sum(n - 1);;
sum 3;; (* should return 6 *)
sum 10;; (* should return 55 *)

(* question 2 *)
let add2 (x : int * int) : int = fst x + snd x;;

add2 (1, 3);; (* should return 4 *)
add2 (6, -1);; (* should return 5*)

(* question 3 *)
type tree = 
  | Empty
  | Leaf of int
  | Node of tree * tree;;

(* question 4 *)
let rec tree_size (t : tree) : int = match t with
  | Empty -> 0
  | Leaf (x) -> 1
  | Node (left, right) -> tree_size left + tree_size right + 1;;

let rec tree_map (t : tree) f () = match t with
  | Empty -> ()
  | Leaf (x) -> f x
  | Node (left, right) -> 
    tree_map left
    tree_map right;;

print_string "Hello world!\n";;
