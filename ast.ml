type var = string;;

type aexp = Num of int 
 | Var of var 
 | Add of aexp * aexp 
 | Mult of aexp * aexp 
 | Sub of aexp * aexp
 | Shl of aexp * aexp
 | Shr of aexp * aexp;;

type bexp = True 
 | False 
 | Aeq of aexp * aexp
 | Beq of bexp * bexp
 | Gte of aexp * aexp
 | Neg of bexp
 | And of bexp * bexp;;

type stm = Ass of var * aexp 
 | Skip 
 | Comp of stm * stm 
 | If of bexp * stm * stm 
 | While of bexp * stm
 | Do_While of stm * bexp;;

type state = var -> int ;;

(* test case*) 
(* let test0 = Ass ("x", Num 5);; *)
let test1 = Skip;;
let test2 = Comp (Ass ("x", Num 3), Ass ("x", Add(Var "x", Num 1)));; 
let test3 = If(Neg(Aeq(Var "x", Num 1)),Ass ("x", Num 3),Ass ("x", Num 7));;
let test4 = Comp (Ass("y", Num 1), While(Neg(Aeq(Var "x", Num 0)),Comp(Ass("y", Mult(Var "y", Var "x")),Ass("x", Sub(Var "x", Num 1)))));; 
let test5 = Comp(Ass("a", Num 84), Comp(Ass("b",Num 22), Comp(Ass("c",Num 0), While(Neg(Aeq(Var "b", Num 0)), Comp(Ass("a", Shl(Var "a", Num 1)), Ass("b", Shr(Var "b", Num 1)))))))
(* let testshl = Ass ("x", Shl(Num 100, Num 3));;
let testshr = Ass ("x", Shr(Num 100, Num 3));;
let test6 = While(Aeq(Var "x", Num 1), Ass ("x", Num 55));;
let test7 = If(Aeq(Var "x", Num 1), Ass ("x", Num 55), Ass ("x", Num 1));;
let test8 = Do_While(Ass("x", Sub(Var "x", Num 1)), Neg(Aeq(Var "x", Num 1)));; *)


