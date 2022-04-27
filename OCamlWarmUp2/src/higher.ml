open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let contains_elem lst e =
  fold (fun a elem -> if elem = e then true else a) false lst
;;

let is_present lst x = 
  rev (fold (fun a elem -> if elem = x then 1::a else 0::a) [] lst)
;;

let count_occ lst target = 
  fold (fun acc elem -> if elem = target then acc + 1 else acc) 0 lst
;;

let uniq lst = 
  fold (fun arr elem -> if (contains_elem arr elem) then arr else elem :: arr) [] lst
;;

let assoc_list lst = 
  fold (fun acc elem -> (elem, (count_occ lst elem))::acc) [] (uniq lst)
;;

let ap fns args = 
  let lst = rev (fold (fun a elem -> map elem args::a) [] fns) in
  rev(fold (fun acc elem -> fold (fun acc e -> e::acc) acc elem) [] lst)
;;