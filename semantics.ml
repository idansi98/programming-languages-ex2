(* solve_a: aexp -> state -> int *) 
let rec solve_a e s = match e with
 Ast.Num m -> m 
 | Var x -> s x 
 | Add (e1, e2) -> solve_a e1 s + solve_a e2 s
 | Mult (e1, e2) -> solve_a e1 s * solve_a e2 s
 | Sub (e1, e2) -> solve_a e1 s - solve_a e2 s
 | Shr (e1, e2) -> solve_a e1 s lsr solve_a e2 s
 | Shl (e1, e2) -> solve_a e1 s lsl solve_a e2 s;;

let not x = 
    match x with
      "tt" -> "ff"
    | "ff" -> "tt"
    | _ -> failwith "not";;


 (* solve_b: bexp -> state -> bool *) 
 let rec solve_b e s = match e with
    Ast.True -> "tt"
    | False -> "ff"
    | Neg (e1) -> not (solve_b e1 s)
    | Beq (e1 ,e2) -> if (solve_b e1 s = solve_b e2 s) then "tt" else "ff"
    | Aeq (e1, e2) -> if (solve_a e1 s == solve_a e2 s) then "tt" else "ff"
    | Gte (e1, e2) -> if (solve_a e1 s >= solve_a e2 s) then "tt" else "ff"
    | And (e1, e2) -> if ((solve_b e1 s = solve_b e2 s) && (solve_b e1 s = "tt")) then "tt" else "ff";;

(* state update : to get a new state *) 
let update x e s = fun y -> if y=x then solve_a e s else s y;; 

exception NotFound of string 
let default_state x = (* 0, default value? *) 
 raise (NotFound "undefined variable");; 

 (* example of an initial state *) 
let s0 = update "x" (Num 1) default_state;; 
let s1 = update "x" (Num 5) default_state;; 
