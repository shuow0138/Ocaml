open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q;;

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}
;;
(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

let rec move_helper (nfa_delta:('q, 's) transition list) (qs: 'q list) (s: 's option) = 
  match nfa_delta with 
  |[] -> []
  |h :: t -> 
    match h with 
    |(a,b,c) -> 
      if ((List.mem a qs) && b = s) 
        then c::(move_helper t qs s)
      else move_helper t qs s  
;; 

let remove_duplicates xs = 
  let cons_uniq xs x = 
    if List.mem x xs then xs else x :: xs in
  List.rev (List.fold_left cons_uniq [] xs);;

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  remove_duplicates (move_helper nfa.delta qs s)
;; 

let sort_lst lst = 
  let compares x y = if x < y then -1 else if x > y then 1 else 0 in
  sort compares lst;;
;;
let rec e_closure_helper (nfa: ('q,'s) nfa_t) (qs: 'q list) = 
 if eq (union (move nfa qs None) qs) qs then 
  union (move nfa qs None) qs 
else 
  e_closure_helper nfa (union (move nfa qs None) qs)
;; 

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  remove_duplicates (e_closure_helper nfa qs)
;;

let rec check_final fs curr = 
  match fs with 
  |[] -> false
  |h::t-> 
    if (List.mem h curr) = true then 
      true 
  else 
    check_final t curr        
;;

let rec accept_helper nfa curr lst= 
  match lst with
  |[]-> if (check_final nfa.fs curr) then true else false
  |h::t ->  
    if e_closure nfa ( move nfa curr h ) = [] then 
    false 
else 
  accept_helper nfa (e_closure nfa (move nfa curr h)) t
;;

let rec lst_converter lst = 
  match lst with 
  |[] ->[]
  |h::t -> (Some h):: lst_converter t
;;

let accept (nfa: ('q,char) nfa_t) (s: string) : bool =  
  accept_helper nfa (e_closure nfa [nfa.q0]) (lst_converter (explode s))
;;
(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let rec new_states_helper nfa nfa_sigma qs = 
  match nfa_sigma with 
  |[] -> []
  | h::t -> (e_closure nfa (move nfa (e_closure nfa qs) (Some h))) :: new_states_helper nfa t qs
;;

let rec new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  new_states_helper nfa nfa.sigma qs
;;

let rec new_trans_helper nfa nfa_sigma qs = 
  match nfa_sigma with 
  |[] -> []
  | h::t -> 
    (qs, (Some h), e_closure nfa (move nfa (e_closure nfa qs) (Some h))) :: new_trans_helper nfa t qs
;;
let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  (*match qs with 
  | [] -> []
  | h'::t' -> 
    ((e_closure nfa qs), (Some h'), e_closure nfa (move nfa (e_closure nfa qs) (Some h')))::temp nfa t' qs*)
    new_trans_helper nfa nfa.sigma qs
  ;;

  let rec new_fianls_helper (nfa: ('q,'s) nfa_t) (qs: 'q list) =
    match qs with 
  | [] -> false
  | h::t -> 
    if List.mem h nfa.fs then 
    true || new_fianls_helper nfa t
  else false || new_fianls_helper nfa t
;;

let rec new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  if new_fianls_helper nfa qs then 
    qs::[]
  else []
;;

let helper (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t) m : ('q list, 's) nfa_t= 
   {
    sigma = nfa.sigma;
    qs = union dfa.qs [m];
    q0 = e_closure nfa [nfa.q0];
    fs = union dfa.fs (new_finals nfa m);
    delta= union dfa.delta (new_trans nfa m);
  } 
;;

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  match work with 
  |[] -> dfa
  |h::t -> 
    fold_left (fun acc x-> if (elem (e_closure nfa x) (helper nfa acc h).qs)  
    then (helper nfa acc h) 
else 
  nfa_to_dfa_step nfa (helper nfa acc h) [e_closure nfa x]) dfa (new_states nfa h)
  
;;

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let dfa = {
    sigma= nfa.sigma; 
    qs= []; 
    q0= e_closure nfa [nfa.q0]; 
    fs= []; 
    delta= []; 
  } in 
  nfa_to_dfa_step nfa dfa [e_closure nfa [nfa.q0]]
;;
