(* question 1 *)
let rec sum (n : int) : int = if n = 0 then 0 else n + sum(n - 1);;
sum 3;; (* should return 6 *)
sum 10;; (* should return 55 *)

(* question 2 *)
let add2 (x : int * int) : int = fst x + snd x;;

add2 (1, 3);; (* should return 4 *)
add2 (6, -1);; (* should return 5*)

(* question 3 *)
(* Data type to represent a tree *)
type tree = 
  | Leaf of int
  | Node of tree * tree;;

(* question 4 *)
(* A function to count the number of nodes in a tree *)
let rec tree_size (t : tree) : int = match t with
  | Leaf (x) -> 1
  | Node (left, right) -> tree_size left + tree_size right + 1;;

(* question 5 *)
(* A function that applies function f to every node in tree t *)
let rec tree_map (t: tree) f : tree = match t with
  | Leaf (x) -> Leaf(f x);
  | Node (left, right) -> Node (tree_map left f, tree_map right f);;

(* --------------------------------------------------------------------------------------- *)
(* Test cases *)

(* question 1 *)
sum 6;; (* should return 21 *)
sum 20;; (* should return 210 *)

(* question 2 *)
add2 (1, 3);; (* should return 4 *)
add2 (10, -1);; (* should return 9*)


(* question 3 *)

(* Building a tree *)
let _4 = Leaf(4);;
let _5 = Leaf(5);;
let _6 = Leaf(6);;
let _7 = Leaf(7);;
let _2 = Node(_4, _5);;
let _3 = Node(_6, _7);;
let root = Node(_2, _3);;


(* question 4 *)
print_int (tree_size (root));;


(* question 5 *)

(* utility function to print a tree *)
let rec print_tree (t: tree) : unit = match t with
  | Leaf(x) -> 
      print_int x;
      print_string "|"
  | Node(left, right) -> 
      print_tree left;
      print_string "|";
      print_tree right;;
      
(* mapper function *)
let callback (x : int) : int = x * 2;;

(* printing original tree *)
print_tree root;;
(* creating a new tree using tree_map *)
let new_tree = tree_map root callback;;
(* printing the new tree *)
print_tree new_tree;;