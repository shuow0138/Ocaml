open Funs

(*************************************)
(* Part 2: Three-Way Search Tree *)
(*************************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int option * int_tree * int_tree * int_tree 
;;
let empty_int_tree = IntLeaf

let rec int_insert x t =
  match t with 
  |IntLeaf -> IntNode(x, None, IntLeaf, IntLeaf, IntLeaf)
  |IntNode(num1, None, l1, l2, l3) -> 
    if x != num1 then 
      if x < num1 then IntNode(x, Some num1, IntLeaf, IntLeaf, IntLeaf) 
      else IntNode(num1, Some x, IntLeaf, IntLeaf, IntLeaf) 
    else  IntNode(num1, None, IntLeaf, IntLeaf, IntLeaf)
  |IntNode(num1,(Some num2),l1,l2,l3) -> 
    if x < num1 then IntNode(num1, Some num2, int_insert x l1, l2, l3) 
    else if x > num2 then IntNode(num1, Some num2, l1, l2, int_insert x l3)
    else IntNode(num1, Some num2, l1, int_insert x l2, l3)
;;

let rec int_mem x t =
  match t with 
  |IntLeaf -> false
  |IntNode(num1, None, l1, l2, l3) -> 
    if x != num1 then false else true
  |IntNode(num1,Some num2,l1,l2,l3) -> 
    if x = num1 || x = num2 then true 
    else 
      if x < num1 then int_mem x l1
      else if x > num2 then int_mem x l3
      else int_mem x l2
;;
let rec int_size t =
  match t with 
  |IntLeaf -> 0
  |IntNode(num1, None, l1, l2, l3) -> 1
  |IntNode(num1,Some num2,l1,l2,l3) -> 
    2 + int_size l1 + int_size l2 + int_size l3
;;

let rec int_max t =
  match t with 
  |IntLeaf -> raise (Invalid_argument("int_max"))
  |IntNode(num1, None, IntLeaf, IntLeaf, IntLeaf) -> num1
  |IntNode(num1, num2,l1,l2,l3) -> 
    if l3 = IntLeaf then match num2 with 
      |None -> 0
      |Some num -> num
    else int_max l3
;;

(*******************************)
(* Part 3: Three-Way Search Tree-Based Map *)
(*******************************)

type 'a tree_map =
  | MapLeaf
  | MapNode of (int * 'a) * (int * 'a) option * 'a tree_map * 'a tree_map * 'a tree_map
;;
let empty_tree_map = MapLeaf

let rec map_put k v t = 
  match t with 
  |MapLeaf -> MapNode((k,v), None, MapLeaf, MapLeaf, MapLeaf)
  |MapNode((num1,v1), None, l1, l2, l3) -> 
    if k != num1 then 
      if k < num1 then 
        MapNode((k,v), Some(num1, v1), MapLeaf, MapLeaf, MapLeaf) 
      else 
        MapNode((num1,v1), Some(k,v), MapLeaf, MapLeaf, MapLeaf) 
    else  
      raise(Invalid_argument("map_put"))
  |MapNode((num1,v1),Some(num2,v2),l1,l2,l3) -> 
    if k < num1 then 
      MapNode((num1,v1), Some(num2,v2), map_put k v l1, l2, l3) 
    else if k = num1 then 
      raise(Invalid_argument("map_put"))
    else if k > num2 then 
      MapNode((num1,v1), Some(num2,v2), l1, l2, map_put k v l3)
    else if k = num2 then 
      raise(Invalid_argument("map_put"))
    else 
      MapNode((num1, v1), Some(num2,v2), l1, map_put k v l2, l3)
;;

let rec map_contains k t = 
  match t with 
  |MapLeaf -> false
  |MapNode((num1,v1), None, l1, l2, l3) -> 
    if k != num1 then false else true
  |MapNode((num1,v1),Some (num2,v2),l1,l2,l3) -> 
    if k = num1 || k = num2 then true 
    else 
      if k < num1 then map_contains k l1
      else if k > num2 then map_contains k l3
      else map_contains k l2
;;

let rec map_get k t =
  match t with 
  |MapLeaf -> raise(Invalid_argument("map_get"))
  |MapNode((num1,v1), None, l1, l2, l3) -> 
    if k != num1 then raise(Invalid_argument("map_get")) else v1
  |MapNode((num1,v1),Some (num2,v2),l1,l2,l3) -> 
    if k = num1 then v1
    else if k = num2 then v2
    else 
      if k < num1 then map_get k l1
      else if k > num2 then map_get k l3
      else map_get k l2
;;
(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)
type lookup_table = (string * int) list list

let empty_table : lookup_table = []

let push_scope (table : lookup_table) : lookup_table = 
  [] :: table 

let pop_scope (table : lookup_table) : lookup_table =
  match table with 
  |[] -> failwith "No scopes remain!"
  |h::t -> t 
;;

let contains_elem lst name =
  fold (fun a elem -> match elem with 
  |(n,value)-> if n = name then true else a) false lst
;;

let add_var name value (table : lookup_table) : lookup_table =
  match table with 
  |[] -> failwith "There are no scopes to add a variable to!"
  |h::t -> 
      if contains_elem h name
        then failwith "Duplicate variable binding in scope!" 
      else 
        ([(name, value)]@h)::t
;;


let rec lookup name (table : lookup_table) =
  match table with 
  |[] -> failwith "Variable not found!"
  |h::t -> if contains_elem h name then
    let rec find_int lst n = 
      match lst with
      |[] -> failwith "Variable not found!"
      |(n,v)::t -> if n = name then 
        v 
    else 
      find_int t name
      in find_int h name
    else 
    lookup name t
;;