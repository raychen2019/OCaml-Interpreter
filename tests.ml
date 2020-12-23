(*
                         CS 51 Final Project
                             Refs Testing
 *)

(* Make your refs solution available for testing *)
open Evaluation ;;
open Expr ;;

(* Creating Sample Expressions to evaluate*)
let expr0 = Num(3);; (*3*)
let expr1 = Let("x", Num(5), Var("x")) ;; (*let x = 5 in x -> 5*)
let expr2 = Let("x", Num(5), Let("y", Num(10), Var("x")));; (* let x = 5 in let y = 10 in x -> 5 *)

(*let x = 5 in let x = x + 5 in x -> 10*)
let expr3 = Let("x", Num(5), Let("x", Binop(Plus, Var("x"), Num(5)), Var("x")))  ;;
(*let x = fun x -> x in x 1 -> 1 *)
let expr4 = Let("x", Fun("x", Var("x")), App(Var("x"), Num(1))) ;;
(* let x = 5 in let f = fun y -> x + y in f f 10 -> 20 *)
let expr5 = Let("x", Num(5), Let("f", Fun("y", Binop(Plus, Var("x"), Var("y"))),  
            App(App(Var("f"), Var("f")), Num(10)))) ;;
(*let f = fun x -> x + 1 in f f f f f 5 -> 10 *)
let expr6 = Let("f", Fun("x", Binop(Plus, Var("x"), Num(1))), 
            App(App(App(App(App(Var("f"), Var("f")), Var("f")), Var("f")), Var("f")), Num(5))) ;;
(*let x = 5 in let rec x = 10 in x -> 10 *)
let expr7 = Let("x", Num(5), Letrec("x", Num(10), Var("x"))) ;;
(*let rec x = 10 in let x = 5 in x -> 5 *)
let expr8 = Letrec("x", Num(10), Let("x", Num(5), Var("x"))) ;;
(*let x = 10 in let x = fun y -> y * 2 in x 20 -> 40 *)
let expr9 = Let("x", Num(10), Let("x", Fun("y", Binop(Times, Var("y"), Num(2))),
 App(Var("x"), Num(20)))) ;;
(*let rec f = fun x -> if x = 0 then 1 else x * f (x-1) in f 4 -> 24 *)
let expr10 = Letrec("f", Fun("x", Conditional(Binop(Equals, Var("x"), Num(0)),
             Num(1), Binop(Times, Var("x"), App(Var("f"), Binop(Minus, Var("x"),
            Num(1)))))), App(Var("f"), Num(4))) ;;
(*let x = 5 in let f = fun y -> x + y in let x = 10 in f 0  *)
let expr11 =  Let("x", Num(5), Let("f", Fun("y", Binop(Plus, Var("x"), 
              Var("y"))), Let("x", Num(10), App(Var("f"), Num(0))))) ;;

let empty_env = Evaluation.Env.empty() ;;

let _ =

  (* Testing substitute / eval_s *)
  assert((eval_s expr0 empty_env) = Val (Num (3)));
  assert((eval_s expr1 empty_env) = Val (Num (5)));
  assert((eval_s expr2 empty_env) = Val (Num (5)));
  assert((eval_s expr3 empty_env) = Val (Num (10)));
  assert((eval_s expr4 empty_env) = Val (Num (1)));
  assert((eval_s expr5 empty_env) = Val (Num (20)));
  assert((eval_s expr6 empty_env) = Val (Num (10)));
  assert((eval_s expr7 empty_env) = Val (Num (10)));
  assert((eval_s expr8 empty_env) = Val (Num (5)));
  assert((eval_s expr9 empty_env) = Val (Num (40)));
  assert((eval_s expr10 empty_env) = Val (Num (24)));
  assert((eval_s expr11 empty_env) = Val (Num (5)));

  (* Testing dynamic / eval_d *)
  assert((eval_d expr0 empty_env) = Val (Num (3)));
  assert((eval_d expr1 empty_env) = Val (Num (5)));
  assert((eval_d expr2 empty_env) = Val (Num (5)));
  assert((eval_d expr3 empty_env) = Val (Num (10)));
  assert((eval_d expr4 empty_env) = Val (Num (1)));
  assert((eval_d expr5 empty_env) = Val (Num (20)));
  assert((eval_d expr6 empty_env) = Val (Num (10)));
  assert((eval_d expr7 empty_env) = Val (Num (10)));
  assert((eval_d expr8 empty_env) = Val (Num (5)));
  assert((eval_d expr9 empty_env) = Val (Num (40)));
  assert((eval_d expr10 empty_env) = Val (Num (24)));
  assert((eval_d expr11 empty_env) = Val (Num (10)));

  (* Testing dynamic / eval_d *)
  assert((eval_l expr0 empty_env) = Val (Num (3)));
  assert((eval_l expr1 empty_env) = Val (Num (5)));
  assert((eval_l expr2 empty_env) = Val (Num (5)));
  assert((eval_l expr3 empty_env) = Val (Num (10)));
  assert((eval_l expr4 empty_env) = Val (Num (1)));
  assert((eval_l expr5 empty_env) = Val (Num (20)));
  assert((eval_l expr6 empty_env) = Val (Num (10)));
  assert((eval_l expr7 empty_env) = Val (Num (10)));
  assert((eval_l expr8 empty_env) = Val (Num (5)));
  assert((eval_l expr9 empty_env) = Val (Num (40)));
  assert((eval_l expr10 empty_env) = Val (Num (24)));
  assert((eval_l expr11 empty_env) = Val (Num (5)));

