(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
;;
  
(*......................................................................
  Manipulation of variable names (varids)
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars :  varidset -> varidset -> bool
   Test to see if two sets of variables have the same elements (for
   testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list : string list -> varidset
   Generate a set of variable names from a list of strings (for
   testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars : expr -> varidset
   Return a set of the variable names that are free in expression
   exp *)
let rec free_vars (exp : expr) : varidset =
  match exp with 
  | Var x -> (SS.add x SS.empty)                                           (* variables *)
  | Num integerVal -> SS.empty            (* integers*)
  | Bool boolVal ->  SS.empty               (* boolean *)
  | Unop (unopVal , exprVal) ->                           (* unary operators, assume only negate *)
    free_vars exprVal                  
  | Binop (binopVal, exprLeft, exprRight) ->              (* binary operators *)
    SS.union (free_vars exprLeft) (free_vars exprRight)
  | Conditional (exprIf, exprThen, exprElse)->            (* if then else, potential issue between the then and else statement *)
    SS.union (SS.union (free_vars exprIf) (free_vars exprThen)) (free_vars exprElse)  
  | Fun (varidVal, exprVal) ->                            (* function definitions *)
    SS.remove varidVal (free_vars exprVal)                    
  | Let (varidVal, exprHead, exprBody) ->                 (* local naming *)
    SS.union  (SS.remove varidVal (free_vars exprBody))
              (free_vars exprHead)
  | Letrec (varidVal, exprHead, exprBody) ->               (* recursive local naming *)
    SS.union  (SS.remove varidVal (free_vars exprBody))
              (SS.remove varidVal (free_vars exprHead))
  | Raise  -> SS.empty                                    (* exceptions *)
  | Unassigned -> SS.empty                (* (temporarily) unassigned *)
  | App (expr1 , expr2) ->                                  (* function applications *)
    SS.union (free_vars expr1) (free_vars expr2) (* function applications *)
  ;;
  
(* new_varname : unit -> varid
   Return a fresh variable, constructed with a running counter a la
   gensym. Assumes no variable names use the prefix "var". (Otherwise,
   they might accidentally be the same as a generated variable name.) *)
let new_varname =
  let ctr = ref 0 in
    fun () ->  (* the old var is the input, and just add a num*)
      let temp =  "newvar" ^ (string_of_int !ctr) in 
      ctr := !ctr + 1; 
    temp;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst : varid -> expr -> expr -> expr
   Substitute repl for free occurrences of var_name in exp *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  (* if (SS.mem var_name (free_vars repl) ) then (* If there is a free variable to replace *) you are substituting more than free variables*)
  match repl with 
  | Var x -> if (x = var_name) then exp else repl                                          (* variables *)
  | Num _ | Bool _ -> repl                                (* Unaffected constants *)
  | Unop (unopVal , exprVal) ->                           (* unary operators, assume only negate *)
    Unop (unopVal, (subst var_name exprVal exp))                  
  | Binop (binopVal, exprLeft, exprRight) ->              (* binary operators *)
    Binop (binopVal, (subst var_name exprLeft exp), 
           (subst var_name exprRight exp))
  | Conditional (exprIf, exprThen, exprElse)->            (* if then else, potential issue between the then and else statement *)
    Conditional ((subst var_name exprIf exp), 
                 (subst var_name exprThen exp),
                 (subst var_name exprElse exp))  
  | Fun (oldVaridVal, exprVal) ->                            (* function definitions *)
    if oldVaridVal = var_name then  
      Fun (oldVaridVal, exprVal)
    else if (SS.mem oldVaridVal (free_vars exp)) then
      let newVarId = new_varname () in
      Fun(newVarId, (subst (var_name) (subst (oldVaridVal) (exprVal) (Var newVarId))) (exp))
    else 
      Fun(oldVaridVal, (subst var_name exprVal exp))
  | Let (oldVaridVal, exprHead, exprBody) ->                 (* local naming *)
    if oldVaridVal = var_name then  
      Let (oldVaridVal, (subst var_name exprHead exp), exprBody)
    else if (SS.mem oldVaridVal (free_vars exp)) then
      let newVarId = new_varname () in
      Let(newVarId, (subst var_name exprHead exp), 
         (subst var_name (subst oldVaridVal exprBody (Var newVarId)) exp))
    else 
      Let(oldVaridVal, (subst var_name exprHead exp), (subst var_name exprBody exp) )
  | Letrec (oldVaridVal, exprHead, exprBody) ->               (* recursive local naming *)
    if oldVaridVal = var_name then  
      Letrec (oldVaridVal, exprHead, exprBody)
    else if (SS.mem oldVaridVal (free_vars exp)) then
      let newVarId = new_varname () in
      Letrec (newVarId, (subst var_name (subst oldVaridVal exprHead (Var newVarId)) exp), 
        (subst var_name (subst oldVaridVal exprBody (Var newVarId)) exp))
    else 
      Letrec (oldVaridVal, (subst var_name exprHead exp), (subst var_name exprBody exp) )
  | Raise  -> Raise                                    (* exceptions *)
  | Unassigned -> Unassigned                (* (temporarily) unassigned *)
  | App (expr1 , expr2) ->                                   (* function applications *)
    App ((subst var_name expr1 exp),(subst var_name expr2 exp)) (* function applications *)
  (*else
  repl*)    
  
  ;;

(*......................................................................
  String representations of expressions
 *)
   
(* Matching the binops to their corresponding symbols
*)
let binop_to_concrete_string (input: binop) : string =
  match input with 
  | Plus -> " + "
  | Minus -> " - "
  | Times -> " * "
  | Equals -> " = "
  | LessThan -> " < "

 
(* exp_to_concrete_string : expr -> string
   Returns a concrete syntax string representation of the expr *)
let rec exp_to_concrete_string (exp : expr) : string =
  match exp with
  | Var x -> x                                            (* variables *)
  | Num integerVal -> string_of_int integerVal            (* integers*)
  | Bool boolVal ->  Bool.to_string boolVal               (* boolean *)
  | Unop (unopVal , exprVal) ->                           (* unary operators, assume only negate *)
    "-"^(exp_to_concrete_string exprVal)                  
  | Binop (binopVal, exprLeft, exprRight) ->              (* binary operators *)
    (exp_to_concrete_string exprLeft)^
    (binop_to_concrete_string binopVal)^
    (exp_to_concrete_string exprRight)
  | Conditional (exprIf, exprThen, exprElse)->            (* if then else *)
    "if "^(exp_to_concrete_string exprIf)^" then "^
    (exp_to_concrete_string exprThen)^" else "^
    (exp_to_concrete_string exprElse)                     
  | Fun (varidVal, exprVal) ->                            (* function definitions *)
    "fun "^varidVal^" -> "^
    (exp_to_concrete_string  exprVal)                     
  | Let (varidVal, exprHead, exprBody) ->                 (* local naming *)
    "let "^varidVal^" = "^
    (exp_to_concrete_string exprHead)^" in "^ 
    (exp_to_concrete_string exprBody)
  | Letrec (varidVal, exprHead, exprBody) ->               (* recursive local naming *)
    "let rec "^varidVal^" = "^
    (exp_to_concrete_string exprHead)^" in "^ 
    (exp_to_concrete_string exprBody)
  | Raise  -> "raising exception"                           (* exceptions *)
  | Unassigned -> "expression is unassigned"                (* (temporarily) unassigned *)
  | App (expr1 , expr2) ->                                  (* function applications *)
    (exp_to_concrete_string expr1)^(exp_to_concrete_string expr2)                                 (* function applications *)


let binop_to_abstract_string (input: binop) : string =
  match input with 
  | Plus -> "Binop(Plus, "
  | Minus -> "Binop(Minus, "
  | Times -> "Binop(Times, "
  | Equals -> "Binop(Equals, "
  | LessThan -> "Binop(LessThan, "
(* exp_to_abstract_string : expr -> string
   Returns a string representation of the abstract syntax of the expr *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with
  | Var x -> "Var("^x^")"                                   (* variables *)
  | Num integerVal -> "Num("^(string_of_int integerVal)^")" (* integers*)
  | Bool boolVal ->  "Bool("^(Bool.to_string boolVal)^")"   (* boolean *)
  | Unop (unopVal , exprVal) ->                             (* unary operators, assumes only negation *)
    "Unop(Negate, "^(exp_to_abstract_string exprVal)^")"                  
  | Binop (binopVal, exprLeft, exprRight) ->                (* binary operators *)
    (binop_to_abstract_string binopVal)^
    (exp_to_abstract_string exprLeft)^", "^
    (exp_to_abstract_string exprRight)^")"
  | Conditional (exprIf, exprThen, exprElse)->              (* if then else *)
    "Conditional("^(exp_to_abstract_string exprIf)^", "^
    (exp_to_abstract_string exprThen)^", "^
    (exp_to_abstract_string exprElse)^")"                     
  | Fun (varidVal, exprVal) ->                              (* function definitions *)
    "Fun("^varidVal^", "^
    (exp_to_abstract_string  exprVal)^")"                     
  | Let (varidVal, exprHead, exprBody) ->                   (* local naming *)
    "Let("^varidVal^", "^
    (exp_to_abstract_string exprHead)^", "^ 
    (exp_to_abstract_string exprBody)^")"
  | Letrec (varidVal, exprHead, exprBody) ->                (* recursive local naming *)
    "Letrec("^varidVal^", "^
    (exp_to_abstract_string exprHead)^", "^ 
    (exp_to_abstract_string exprBody)^")"
  | Raise  -> "Raise"                                       (* exceptions *)
  | Unassigned -> "Unassigned"                              (* (temporarily) unassigned *)
  | App (expr1 , expr2) ->                                  (* function applications *)
    "App("^(exp_to_abstract_string expr1)^", "^
    (exp_to_abstract_string expr2)^")"                      (* function applications *)

