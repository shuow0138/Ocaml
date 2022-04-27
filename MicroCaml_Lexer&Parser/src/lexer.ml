open TokenTypes
open String
(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let re_rparen = Str.regexp ")";;
let re_lparen = Str.regexp "(";;
let re_equ = Str.regexp "=";;
let re_notequ = Str.regexp "<>";;
let re_greater = Str.regexp ">";;
let re_less = Str.regexp "<";;
let re_greaterequ = Str.regexp ">=";;
let re_lessequ = Str.regexp "<=";;
let re_or= Str.regexp "||";;
let re_and = Str.regexp "&&";;
let re_not = Str.regexp "not";;
let re_if = Str.regexp "if";;
let re_then = Str.regexp "then";;
let re_else = Str.regexp "else";;
let re_add = Str.regexp "+";;
let re_sub = Str.regexp "-";;
let re_mul = Str.regexp "*";;
let re_div = Str.regexp "/";;
let re_concat = Str.regexp "\\^";;(*here*)
let re_let = Str.regexp "let";;
let re_rec = Str.regexp "rec";;
let re_in = Str.regexp "in";;
let re_def = Str.regexp "def";;
let re_fun = Str.regexp "fun";;
let re_arrow = Str.regexp "->";;
let re_whitespace = Str.regexp "[ \t\n]+";;
let re_doublesemi = Str.regexp ";;";;
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*";;
let re_str = Str.regexp "\"[^\"]*\"";;

let re_pos_int = Str.regexp "[0-9]+";;
let re_neg_int = Str.regexp "(-[0-9]+)";;
let re_bool = Str.regexp "(true\\|false)";;

      
        (*
      else if (Str.string_match re_not s pos) then 
        Tok_Not :: (tok (pos + String.length "not") s) 
      else if (Str.string_match re_if s pos) then 
        Tok_If :: (tok (pos + String.length "if") s) 
      else if (Str.string_match re_then s pos) then 
        Tok_Then :: (tok (pos + String.length "then") s) 
      else if (Str.string_match re_else s pos) then 
        Tok_Else :: (tok (pos + String.length "else") s) 
        *)
        (*
      else if (Str.string_match re_let s pos) then 
        Tok_Let :: (tok (pos + String.length "let") s) 
      else if (Str.string_match re_rec s pos) then 
        Tok_Rec :: (tok (pos + String.length "rec") s) 
      else if (Str.string_match re_in s pos) then 
        Tok_In :: (tok (pos + String.length "in") s) 
      else if (Str.string_match re_def s pos) then 
        Tok_Def :: (tok (pos + String.length "def") s) 
      else if (Str.string_match re_fun s pos) then 
        Tok_Fun :: (tok (pos + String.length "fun") s) 
        
      
*)
let tokenize input = 
  let rec tok pos s = 
    if pos >= String.length s then
      []
    else
      if (Str.string_match re_rparen s pos) then
        Tok_RParen::(tok (pos + 1) s)
      else if (Str.string_match re_lparen s pos) then
        Tok_LParen::(tok (pos + 1) s)
      else if (Str.string_match re_equ s pos) then
        Tok_Equal::(tok (pos + 1) s)
      else if (Str.string_match re_notequ s pos) then
        Tok_NotEqual::(tok (pos + 2) s)
      else if (Str.string_match re_greater s pos) then
        Tok_Greater::(tok (pos + 1) s)
      else if (Str.string_match re_less s pos) then
        Tok_Less::(tok (pos + 1) s)
      else if (Str.string_match re_greaterequ s pos) then
        Tok_GreaterEqual::(tok (pos + 2) s)
      else if (Str.string_match re_lessequ s pos) then
        Tok_LessEqual::(tok (pos + 2) s)
      else if (Str.string_match re_or s pos) then
        Tok_Or::(tok (pos + 2) s)
      else if (Str.string_match re_and s pos) then
        Tok_And::(tok (pos + 2) s)
      else if (Str.string_match re_add s pos) then
        Tok_Add::(tok (pos + 1) s)
      else if (Str.string_match re_arrow s pos) then
        Tok_Arrow::(tok (pos + 2) s)
      else if (Str.string_match re_sub s pos) then
        Tok_Sub::(tok (pos + 1) s)
     (* else if (Str.string_match re_arrow s pos) then
        Tok_Arrow::(tok (pos + 2) s)*)
      else if (Str.string_match re_mul s pos) then
        Tok_Mult::(tok (pos + 1) s)
      else if (Str.string_match re_div s pos) then
        Tok_Div::(tok (pos + 1) s)
      else if (Str.string_match re_concat s pos) then
        Tok_Concat::(tok (pos + 1) s)
      else if (Str.string_match re_id s pos) then
        let token = Str.matched_string s in
        if token = "not" then 
          Tok_Not::(tok (pos + 3) s)
         else if token = "if" then 
           Tok_If::(tok (pos + 2) s)
        else if token = "then" then 
           Tok_Then::(tok (pos + 4) s)
        else if token =  "else" then 
          Tok_Else::(tok (pos + 4) s)
        else if token =  "let" then 
          Tok_Let::(tok (pos + 3) s)
        else if token =  "rec" then 
          Tok_Rec::(tok (pos + 3) s)
        else if token =  "in" then 
          Tok_In::(tok (pos + 2) s)
        else if token =  "def" then 
          Tok_Def::(tok (pos + 3) s)
        else if token =  "fun" then 
          Tok_Fun::(tok (pos + 3) s)
        else if token =  "true" then 
          let token = Str.matched_string s in
          (Tok_Bool (bool_of_string token))::(tok (pos + 4) s)
        else if token =  "false" then
          let token = Str.matched_string s in
          (Tok_Bool (bool_of_string token))::(tok (pos + 5) s)
        else (Tok_ID token)::(tok (pos + (String.length token)) s) 
      else if (Str.string_match re_doublesemi s pos) then
        Tok_DoubleSemi::(tok (pos + 2) s)
      else if (Str.string_match re_whitespace s pos) then
        let whitespace = Str.matched_string s in
        (tok (pos + (String.length whitespace)) s)
     (* else if (Str.string_match re_int s pos) then 
        let token = Str.matched_string s in
        if token.[0] != '(' then 
        (*need more to distinguish positive and negative numbers*)
          (Tok_Int (int_of_string token)) :: (tok (pos + String.length token) s) 
        else 
          Tok_RParen::Tok_Sub::(Tok_Int (int_of_string token))::Tok_LParen :: (tok (pos + 1 + 1 + String.length token + 1) s) 
          *)
      else if (Str.string_match re_pos_int s pos) then
        let token = Str.matched_string s in
        (Tok_Int (int_of_string token))::(tok (pos + (String.length token)) s)
      else if (Str.string_match re_neg_int s pos) then
          let token = Str.matched_string s in
          let value = String.sub token 1 ((String.length token) - 2) in
          (Tok_Int (int_of_string value))::(tok (pos + (String.length token)) s)
      else if (Str.string_match re_str s pos) then
        let token = Str.matched_string s in
        let value = String.sub token 1 ((String.length token) - 2) in
        (Tok_String value)::(tok (pos + (String.length token)) s)
        (*
      else if (Str.string_match re_bool s pos) then 
        let token = Str.matched_string s in
        if token = "true" then 
         (Tok_Bool true) :: (tok (pos + String.length "true") s) 
        else 
          (Tok_Bool false) :: (tok (pos + String.length "false") s) 
      else if (Str.string_match re_str s pos) then 
        let token = Str.matched_string s in
        (Tok_String token):: (tok (pos + String.length token) s) 
      else if (Str.string_match re_id s pos) then 
        let token = Str.matched_string s in
        (Tok_String token):: (tok (pos + String.length token) s) 
*)
      

      
      
      else
        raise (InvalidInputException "Invalid Token")
      in
      tok 0 input
    ;;