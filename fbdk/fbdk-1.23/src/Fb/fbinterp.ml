open Fbast;;

exception NotClosed;;
exception TypeMismatch;;

let (@:) f x = f x;;

let list_empty lst = 
  match lst with 
  | [] -> true
  | h :: t -> false
;;


(**
 * Determines whether or not an expression is closed
 *
 * expr: Fbast.ident
 *)
let check_closed expr =
  (* helper function *)
  let rec check vars expr = 
    match expr with 
    (* base cases *)
    | Var(ident) -> 
        if List.mem ident vars then true
        else false

    | Int(_) -> true

    | Bool(_) -> true

    | Function(ident, expr) -> 
        check (ident::vars) expr

    | Appl(expr1, expr2) -> 
        check vars expr1 && check vars expr2

    (* Let Rec f x = e1 In e2 
     * f, x are bound in e1
     * f bound in e2 *)
    | LetRec(f, x, expr1, expr2) -> 
        check (f :: x :: vars) expr1 &&
        check (f :: vars) expr2
    
    | Plus(expr1, expr2) ->
        check vars expr1 && check vars expr2

    | Minus(expr1, expr2) -> 
        check vars expr1 && check vars expr2

    | Equal(expr1, expr2) -> 
        check vars expr1 && check vars expr2

    | And(expr1, expr2) ->
        check vars expr1 && check vars expr2

    | Or(expr1, expr2) ->
        check vars expr1 && check vars expr2

    | Not(expr) ->
        check vars expr

    | If(condi, expr1, expr2 ) -> 
        check vars condi && check vars expr1 && check vars expr2
  in
  check [] expr
;;


(**
 * Variable substitution
 *
 * expr : Fbast.expr given expression to be substitued
 * v    : Fbast.expr the value used to substituion
 * ident: Fbast.ident the indent of the varible need to be substituted
 *
 *)
let rec subst expr v ident = 
  match expr with
  | Var(ident_expr) -> 
      if ident_expr = ident then v
      else expr 
  
  | Function(x, body) ->
      (* shadowlling (Function x -> Function x -> x + x) 2 *)
      if x = ident then expr 
      else Function(x, subst body v ident) 

  | Appl(e1, e2) -> Appl(subst e1 v ident, subst e2 v ident)

  (* Let Rec f x = e1 In e2 
     * f, x are bound in e1
     * f bound in e2 *)
  | LetRec(f, x, e1, e2) ->
      if ident != f && ident != x then
        LetRec(f, x, subst e1 v ident, subst e2 v ident)
      else if ident != f then
        LetRec(f, x, e1, subst e2 v ident)
      else
        expr

  | Plus(e1, e2) -> Plus(subst e1 v ident, subst e2 v ident)
  
  | Minus(e1, e2) -> Minus(subst e1 v ident, subst e2 v ident)
  
  | Equal(e1, e2) -> Equal(subst e1 v ident, subst e2 v ident)
      
  | Or(e1, e2) -> Or(subst e1 v ident, subst e2 v ident)

  | And(e1, e2) -> And(subst e1 v ident, subst e2 v ident) 
  
  | Not(e) -> Not(subst e v ident)

  | If(e1, e2, e3) -> If(subst e1 v ident, subst e2 v ident, subst e3 v ident)

  | Bool(_) -> expr

  | Int(_) -> expr
;;

(*
 * Replace this with your interpreter code.
 *)
let eval expr = 
  let rec evaluate expr = 
    match expr with
    | Not(e) -> (
        match evaluate e with
        | Bool(v) -> Bool(not v)
        | _ -> raise TypeMismatch )

    | And(e1, e2) -> (
        match evaluate e1, evaluate e2 with
        | Bool(v1), Bool(v2) -> Bool(v1 && v2)
        | _ -> raise TypeMismatch )
    
    | Or(e1, e2) -> (
        match evaluate e1, evaluate e2 with
        | Bool(v1), Bool(v2) -> Bool(v1 || v2)
        | _ -> raise TypeMismatch )

    | Plus(e1, e2) -> (
        match evaluate e1, evaluate e2 with
        | Int(v1), Int(v2) -> Int(v1+v2)
        | _ -> raise TypeMismatch )

    | Minus(e1, e2) -> (
        match evaluate e1, evaluate e2 with
        | Int(v1), Int(v2) -> Int(v1-v2)
        | _ -> raise TypeMismatch )

    | Equal(e1, e2) ->  (
        match evaluate e1, evaluate e2 with
        | Int(v1), Int(v2) -> if v1 = v2 then Bool(true) else Bool(false)
        | _ -> raise TypeMismatch )

    | If(condi, e1, e2 ) -> (
        match evaluate condi with
        | Bool(v) -> if v then evaluate e1 else evaluate e2
        | _ -> raise TypeMismatch)

    | Appl(e1, e2) -> (
      match evaluate e1, evaluate e2 with
      | Function(x, body),  sub -> evaluate @: subst body sub x
      | _ -> raise TypeMismatch)

    (* 
     * e2[Function x -> e1[(Function x -> Let Rec f x = e1 In f x)/f]/f] ) => v 
     * Let Rec f x = e1 In e2) => v
     * *)
    | LetRec(f, x, e1, e2) ->
        let mk_rec_subt = 
          subst (Function(x, e1)) 
                ( Function(x, LetRec(f, x, e1, Appl(Var(f), Var(x)))) ) 
                f in 
        evaluate @: subst e2 (mk_rec_subt ) f
   
    | Var(_) -> raise NotClosed

    (* Bool, Int, Function *)
    | _ -> expr
  in
  if not (check_closed expr) then raise NotClosed
  else evaluate expr
;;

