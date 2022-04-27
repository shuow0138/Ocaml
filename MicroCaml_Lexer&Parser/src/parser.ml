open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

let get_ID toks = 
  match toks with 
  [] -> raise (InvalidInputException "Empty")
  | h::t -> (match h with
    Tok_ID s -> s
    |Tok_String s -> s
    |_ -> raise (InvalidInputException "No match"))

let rec parse_expr toks = 
  let token = lookahead toks in 
  if token = Some Tok_Let then
    parse_Let toks
  else if token = Some Tok_If then
    parse_If toks
  else if token = Some Tok_Fun then
    parse_Func toks
  else
    parse_Or toks
      (*---------------------------------------------------------*)
and parse_Let toks =
  let tokens = match_token toks Tok_Let in
  let (rec_toks, is_rec) = parse_Rec tokens in
  let tokens = rec_toks in
  let id = get_ID tokens in
  let tokens = match_token tokens (Tok_ID id) in
  let tokens = match_token tokens Tok_Equal in
  let (e1_toks, e1) = parse_expr tokens in
  let tokens = e1_toks in
  let tokens = match_token tokens Tok_In in
  let (e2_toks, e2) = parse_expr tokens in
  let tokens = e2_toks in
  (tokens, Let (id, is_rec, e1, e2))
    (*---------------------------------------------------------*)
and parse_Rec toks = 
  match lookahead toks with
  Some Tok_Rec -> ((match_token toks Tok_Rec), true)
  |_ -> (toks, false)
    (*---------------------------------------------------------*)
and parse_Func toks = 
  let tokens = match_token toks Tok_Fun in
  let id = get_ID tokens in 
  let tokens = match_many tokens [(Tok_ID id); Tok_Arrow;] in
  let (e1_toks, e1) = parse_expr tokens in
  (e1_toks, Fun (id, e1))
    (*---------------------------------------------------------*)
and parse_If toks = 
  let tokens = match_token toks Tok_If in
  let (e1_toks, e1) = parse_expr tokens in
  let tokens = match_token e1_toks Tok_Then in
  let (e2_toks, e2) = parse_expr tokens in
  let tokens = match_token e2_toks Tok_Else in
  let (e3_toks, e3) = parse_expr tokens in
  (e3_toks, If (e1, e2, e3))
    (*---------------------------------------------------------*)
and parse_Or toks =
  let (and_toks, and_expr) = parse_And toks in
  match lookahead and_toks with
  Some Tok_Or -> 
    let tokens = match_token and_toks Tok_Or in
    let (or_toks, or_expr) = parse_Or tokens in
    (or_toks, Binop (Or, and_expr, or_expr))
 |_ -> (and_toks, and_expr)
   (*---------------------------------------------------------*)
and parse_And toks =
  let (equ_toks, equ_expr) = parse_Equality toks in
  match lookahead equ_toks with
  Some Tok_And -> 
    let tokens = match_token equ_toks Tok_And in
    let (and_toks, and_expr) = parse_And tokens in
    (and_toks, Binop (And, equ_expr, and_expr))
  |_ -> (equ_toks, equ_expr)
    (*---------------------------------------------------------*)
and parse_Equality toks = 
(*how to check lookahead*)
  let (rel_toks, rel_expr) = parse_Relational toks in 
  match lookahead rel_toks with
  Some Tok_Equal ->
    let tokens = match_token rel_toks Tok_Equal in
    let (equ_toks, equ_expr) = parse_Equality tokens in
    (equ_toks, Binop (Equal, rel_expr, equ_expr))
  |Some Tok_NotEqual ->
    let tokens = match_token rel_toks Tok_NotEqual in
    let (notequ_toks, notequ_expr) = parse_Equality tokens in
    (notequ_toks, Binop (NotEqual, rel_expr, notequ_expr))
  |_ -> (rel_toks, rel_expr)
    (*---------------------------------------------------------*)
and parse_Relational toks =
  let (add_toks, add_expr) = parse_Addi toks in
  match lookahead add_toks with
  Some Tok_Less ->
    let tokens = match_token add_toks Tok_Less in
    let (less_toks, less_expr) = parse_Relational tokens in
    (less_toks, Binop (Less, add_expr, less_expr))
  |Some Tok_Greater ->
    let tokens = match_token add_toks Tok_Greater in
    let (greater_toks, greater_expr) = parse_Relational tokens in
    (greater_toks, Binop (Greater, add_expr, greater_expr))
  |Some Tok_LessEqual ->
    let tokens = match_token add_toks Tok_LessEqual in
    let (le_toks, le_expr) = parse_Relational tokens in
    (le_toks, Binop (LessEqual, add_expr, le_expr))
  |Some Tok_GreaterEqual ->
    let tokens = match_token add_toks Tok_GreaterEqual in
    let (ge_toks, ge_expr) = parse_Relational tokens in
    (ge_toks, Binop (GreaterEqual, add_expr, ge_expr))
  |_ -> (add_toks, add_expr)
    (*---------------------------------------------------------*)
and parse_Addi toks =
  let (mul_toks, mul_expr) = parse_Mul toks in
  match lookahead mul_toks with
  Some Tok_Add ->
    let tokens = match_token mul_toks Tok_Add in
    let (add_toks, add_expr) = parse_Addi tokens in
    (add_toks, Binop (Add, mul_expr, add_expr))
  |Some Tok_Sub ->
    let tokens = match_token mul_toks Tok_Sub in
    let (sub_toks, sub_expr) = parse_Addi tokens in
    (sub_toks, Binop (Sub, mul_expr, sub_expr))
  |_ -> (mul_toks, mul_expr)
    (*---------------------------------------------------------*)
and parse_Mul toks =
  let (con_toks, con_expr) = parse_Con toks in
  match lookahead con_toks with
  Some Tok_Mult -> 
    let tokens = match_token con_toks Tok_Mult in
    let (mul_toks, mul_expr) = parse_Mul tokens in
    (mul_toks, Binop (Mult, con_expr, mul_expr))
  |Some Tok_Div ->
    let tokens = match_token con_toks Tok_Div in
    let (div_toks, div_expr) = parse_Mul tokens in
    (div_toks, Binop (Div, con_expr, div_expr))
  |_ -> (con_toks, con_expr)
  (*---------------------------------------------------------*)
  and parse_Con toks = 
  let (un_toks, un_expr) = parse_Un toks in
  match lookahead un_toks with
  Some Tok_Concat ->
    let tokens = match_token un_toks Tok_Concat in
    let (con_toks, con_expr) = parse_Con tokens in
    (con_toks, Binop (Concat, un_expr, con_expr))
  |_ -> (un_toks, un_expr)
and parse_Un toks = 
  match lookahead toks with
  Some Tok_Not -> 
    let tokens = match_token toks Tok_Not in
    let (un_toks, un_expr) = parse_Un tokens in
    (un_toks, Not (un_expr))
  |_ -> parse_FunctionCall toks
    (*---------------------------------------------------------*)
and parse_FunctionCall toks =
  let (prim_toks, prim_expr) = parse_Prim toks in
  match lookahead prim_toks with
  Some (Tok_Int i) -> 
    let (int_toks, int_expr) = parse_Prim prim_toks in
    (int_toks, FunctionCall (prim_expr, int_expr))
  |Some (Tok_Bool b) ->
    let (bool_toks, bool_expr) = parse_Prim prim_toks in
    (bool_toks, FunctionCall (prim_expr, bool_expr))
  |Some (Tok_String s)->
    let (str_toks, str_expr) = parse_Prim prim_toks in
    (str_toks, FunctionCall (prim_expr, str_expr))
  |Some (Tok_ID id) ->
    let (id_toks, id_expr) = parse_Prim prim_toks in
    (id_toks, FunctionCall (prim_expr, id_expr))
  |Some Tok_LParen ->
    let (lp_toks, lp_expr) = parse_Prim prim_toks in
    (lp_toks, FunctionCall (prim_expr, lp_expr))
  |_ -> (prim_toks, prim_expr)
  (*---------------------------------------------------------*)
and parse_Prim toks =
  match lookahead toks with
  Some (Tok_Int int) -> 
    let tokens = match_token toks (Tok_Int int) in
    (tokens, Value(Int(int)))
  |Some (Tok_Bool bool) -> 
    let tokens = match_token toks (Tok_Bool bool) in
    (tokens, Value(Bool(bool)))
  |Some (Tok_String str) -> 
    let tokens = match_token toks (Tok_String str) in
    (tokens, Value(String(str)))
  |Some (Tok_ID id) ->  
    let tokens = match_token toks (Tok_ID id) in
    (tokens, ID(id))
  |Some Tok_LParen -> 
    let tokens = match_token toks Tok_LParen in
    let (ptoks, pexpr) = parse_expr tokens in
    let tokens = match_token ptoks Tok_RParen in
    (tokens, pexpr)
  |_ -> raise (InvalidInputException "error")
  


(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  match lookahead toks with
  Some Tok_DoubleSemi ->
    let tokens = match_token toks Tok_DoubleSemi in
    (tokens, NoOp)
  |Some Tok_Def ->
    let tokens = match_token toks Tok_Def in
    let id = get_ID tokens in
    let tokens = match_many tokens [Tok_ID id; Tok_Equal;] in
    let (e_toks, e_expr) = parse_expr tokens in
    let tokens = match_token e_toks Tok_DoubleSemi in
    (tokens, Def (id, e_expr))
  |_ ->
    let (e_toks, e_expr) = parse_expr toks in
    let tokens = match_token e_toks Tok_DoubleSemi in
    (tokens, Expr (e_expr))
  ;;