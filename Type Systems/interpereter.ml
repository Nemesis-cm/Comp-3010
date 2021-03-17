type exp =
    | True
    | False
    | If of exp * exp * exp
    | Num of int
    | IsZero of exp
    | Plus of exp * exp
    | Mult of exp * exp

    type typ =
    | TBool
    | TInt

  exception Eval_error
  exception Type_error

  let rec step (e : exp) = match e with
        | Num x -> raise Eval_error
        | True -> raise Eval_error
        | False -> raise Eval_error


        | Plus (Num x , Num y) ->
          let n1 = x + y in Num n1

        | Plus (e1, Num x) ->
        if e1 = True || e1 = False then raise Eval_error
        else
        let e1' = step e1 in Plus(e1', Num x)

        | Plus (Num x, e1 ) ->
        if e1 = True || e1 = False then raise Eval_error
        else
        let e1' = step e1 in Plus(Num x, e1')

        | Plus (e1,e2) ->
        if e1 = True || e1 = False || e2 = True || e2 = False then raise Eval_error
        else
        let e1' = step e1 in Plus(e1',e2)

        | Mult (Num x, Num y) ->
        let n1 = x * y in Num n1

        | Mult (e1, Num x) ->
        if e1 = True || e1 = False then raise Eval_error
        else
        let e1' = step e1 in Mult(e1', Num x)

        | Mult (Num x, e1 ) ->
        if e1 = True || e1 = False then raise Eval_error
        else
        let e1' = step e1 in Mult(Num x, e1')

        | Mult (e1,e2) ->
        if e1 = True || e1 = False || e2 = True || e2 = False then raise Eval_error
        else
        let e1' = step e1 in Mult(e1',e2)
        
        |IsZero(True) -> raise Eval_error
        |IsZero(False) -> raise Eval_error
        |IsZero(e1) -> let e1' = step e1 in IsZero(e1')
        |If(True,e1,e2) -> e1
        |If (False, e1,e2) -> e2
        |If (e,e1,e2) -> match e with
            |Num x -> raise Eval_error
            | _ -> let e' = step e in(If(e',e1,e2));;
        
        
        


let rec multi_step (e: exp ) = match e with

  | Num x -> Num x
  | True -> True
  | False -> False
  | _ -> let exprafteronestep = step e in
      multi_step exprafteronestep;;


let rec type_check(e : exp) = match e with

| True -> TBool
| False -> TBool
| Num x -> TInt

| IsZero(e1) ->

        if type_check e1 = TBool then raise Type_error
        else if type_check e1 = TInt then TBool
        else let e1' = step e1 in type_check e1'


|Plus(e1,e2) ->

        if type_check e1 = TBool || type_check e2 = TBool then raise Type_error
        else if type_check e1 = TInt && type_check e2 = TInt then TInt
        else if type_check e1 = TInt then let e2' = step e2 in type_check e2'
        else if type_check e2 = TInt then let e1' = step e1 in type_check e1'
        else let e1' = step e1 in type_check e1'

|Mult (e1,e2) ->

        if type_check e1 = TBool || type_check e2 = TBool then raise Type_error
        else if type_check e1 = TInt && type_check e2 = TInt then TInt
        else if type_check e1 = TInt then let e2' = step e2 in type_check e2'
        else if type_check e2 = TInt then let e1' = step e1 in type_check e1'
        else let e1' = step e1 in type_check e1'

|If (e,e1,e2) ->

        if type_check e = TInt || type_check e1 = TBool || type_check e2 = TBool then raise Type_error
        else if type_check e = TBool && type_check e1 = TInt && type_check e2 = TInt then TInt
        else if type_check e = TBool && type_check e1 = TInt then let e2' = step e2 in type_check e2'
        else if type_check e = TBool && type_check e2 = TInt then let e1' = step e1 in type_check e1'
        else let e' = step e in type_check e';;


let rec string_of_exp (e: exp) = match e with

        |IsZero(expression) ->
        "(IsZero)" ^ string_of_exp expression ^ ")"

        |Plus(left, right) ->
        "(" ^ string_of_exp left ^ " + " ^ string_of_exp right ^ ")"

        |Mult (left,right ) ->
        "(" ^ string_of_exp left ^ " * " ^ string_of_exp right ^ ")"

        |If (left,center,right) ->
        "If" ^ string_of_exp left ^ " then " ^ string_of_exp center ^ " else " ^ string_of_exp right

        | True -> "True"
        | False -> "False"
        | Num x -> "(Num " ^ string_of_int x ^ ")";;

let string_of_type (t: typ) = match t with

    |TBool -> "TBool"
    |TInt -> "TInt";;


let print_multi_step e = print_endline(string_of_exp(multi_step e));;

let print_type_check e = print_endline(string_of_type(type_check e));;
