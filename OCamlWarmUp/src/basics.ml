(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = 
  match tup with 
  |(fst, sec, thd) -> (thd, sec, fst)
;;

let is_odd x = 
  if x mod 2 != 0 then 
    true
  else 
    false
;;

let area x y = 
  match x, y with 
  (xfst, xsec), (yfst, ysec) -> let a = (yfst - xfst)*(ysec - xsec) in
  if a < 0 then 
    -1*a
  else 
    a
;;

let volume x y = 
  match x, y with 
  |(xfst, xsec, xthd), (yfst, ysec, ythd) -> let a = (yfst - xfst)*(ysec - xsec)*(ythd - xthd) in
  if a < 0 then 
   -1*a
  else 
    a
;;
(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n = 
  match n with 
  |0 -> 0
  |1 -> 1
  |_ -> (fibonacci (n-1)) + (fibonacci (n-2))
;;

let rec pow x y = 
  match y with 
  |0 -> 1
  |1 -> x
  |_ -> x * pow x (y - 1)
;;

let rec log x y = 
  if x >= y then 
    if x > y then 
    0
    else 
    1
  else 
    1 + log x (y/x)
;;

let rec gcf x y = 
  if y = 0 then 
   x
  else 
    if x > y then 
      gcf (x-y) y 
    else 
      gcf x (y-x)
;;

let rec is_prime x = 
  if x > 1 then
    let rec aux d =
      d * d > x ||  (x mod d != 0) && aux (d+1) in
    aux 2
  else 
    false;;
(*****************)
(* Part 3: Lists *)
(*****************)

let rec get idx lst = 
  match lst with 
  |[] -> failwith "Out of bounds"
  |h::t -> 
    if idx = 0 then
     h 
    else 
    get (idx-1) t
;;

let length list =
  let rec aux n = function
    | [] -> n
    | _ :: t -> aux (n + 1) t
  in aux 0 list
;;

let larger lst1 lst2 = 
let l1 = length lst1 in
let l2  = length lst2 in
if l1 > l2 then
  lst1
else if l2 > l1 then
  lst2
else 
  []
;;

let rec reverse lst = 
  match lst with 
|[] -> []
| h :: t -> (reverse t) @ [h] 
;;

let rec combine lst1 lst2 = 
  match lst1 with 
|[] -> lst2
|h::t -> h :: combine t lst2
;;

let rec merge lst1 lst2 =
  match lst1, lst2 with 
  |[], [] -> []
  |[], h::t -> lst2
  |h::t, [] -> lst1
  |h1::t1, h2::t2 -> if h1 < h2 then 
    h1::h2::(merge t1 t2) 
  else 
    h2::h1::(merge t1 t2) 
;;

  let move1 l =
    let rec iterate acc = function
        [] -> []
      | [x] -> x :: reverse acc
      | x :: l -> iterate (x :: acc) l
    in
    iterate [] l
  ;;

let rec rotate shift lst =  
 match shift with 
 | 0 ->  lst
  | _ -> rotate (shift-1) (move1 lst)
;;

let rec is_palindrome lst = 
  lst = reverse lst 
;;