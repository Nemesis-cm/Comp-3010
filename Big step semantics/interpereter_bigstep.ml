type exp =
  | True
  | False
  | If of exp * exp * exp
  | Num of int
  | IsZero of exp
  | Plus of exp * exp
  | Mult of exp * exp

  (* Exception is implemented *)

  exception Eval_error

  let rec string_of_exp (e : exp) = match e with

  (* Our methods to help compute our given test statements*)

  | IsZero(expression) ->
  "(isZero" ^ string_of_exp expression ^ ")"

  | Plus(left,right) ->
  "(" ^ string_of_exp left ^ " + " ^ string_of_exp right ^")"

  | Mult(left,right) ->
  "(" ^ string_of_exp left ^ " * " ^ string_of_exp right ^")"

  | If (left,center,right) ->
  "if" ^ string_of_exp left ^ "then" ^ string_of_exp center ^ " else " ^ string_of_exp right
  | True -> "true"
  | False -> "false"

  | Num x -> "(Num " ^ string_of_int x ^ ")";;


let rec eval(e: exp) = match e with

| Num x -> Num x
| True -> True
| False -> False
| Plus(Num x, Num y) ->
let n1 = x + y in Num n1

(* Methods to account for all our tests*)

            | Plus(e1,Num x) ->
            if e1 = True || e1 = False then raise Eval_error else
            let e1'= eval e in eval(Plus(e1', Num x))
            
            | Plus (Num x, e1) ->
            if e1 = True || e1 = False then raise Eval_error else
            let e1' = eval e1 in eval(Plus(Num x, e1'))
            
            | Plus (e1,e2) ->
            if e1 = True || e1 = False || e2 = True || e2 = False then raise Eval_error else
            let e1' = eval e1 in eval(Plus(e1',e2))
            
            | Mult (Num x, Num y) ->
            let n1 = x * y in Num n1
            
            | Mult (e1, Num x) ->
            if e1 = True || e1 = False then raise Eval_error else
            let e1' = eval e1 in eval(Mult(e1', Num x))
            
            | Mult (Num x, e1) ->
            if e1 = True || e1 = False then raise Eval_error else
            let e1' = eval e1 in eval(Mult(Num x, e1'))
            
            | Mult (e1,e2) ->
            if e1 = True || e1 = False || e2 = True || e2 = False then raise Eval_error else
            let e1' = eval e1 in eval(Mult (e1',e2))
            
            | IsZero(Num x) ->
            if x = 0 then True
            else if x != 0 then False
            else raise Eval_error
            
            | IsZero(True) ->
            raise Eval_error
            
            | IsZero(False) ->
            raise Eval_error

        | IsZero(e1) -> let e1' = eval e1 in eval(IsZero(e1'))
        | If(True,e1,e2) -> eval e1
        | If(False,e1,e2) -> eval e2
        | If(e,e1,e2) -> match e with
            |Num x -> raise Eval_error
            | _ -> let e' = eval e in eval(If(e', e1, e2));;

(* Here we print our statements*)

let print_expr e = print_endline(string_of_exp e);;

let print_eval e = print_endline(string_of_exp(eval e));;
