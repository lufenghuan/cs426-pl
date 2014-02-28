(* First load all the relevant compiled structs.  These commands assume ocaml
   was launched from the .../src/Fb/ directory.  If not, do a 
   # #cd ".../src/Fb";; to get there.  *)


#load "afbvast.cmo";;
#load "afbvparser.cmo";;
#load "afbvlexer.cmo";;
#load "afbvpp.cmo";;
#load "afbvinterp.cmo";;

(* Make some structs available at the top for easier use *)

open Afbvast;;
open Afbvinterp;;


(* function parse parses FbST concrete syntax you enter as a string *)

let parse s = 
    let lexbuf = Lexing.from_string s in
  	Afbvparser.main Afbvlexer.token lexbuf;;

(* Function pp is a top-loop pretty printer using FBDK's pretty printer *)

let pp e = print_string (Afbvpp.pretty_print e);;

(* ppeval evals then pretty prints the result *)

let ppeval x = print_string "==> ";pp (eval x);;

(* function rep is a read-eval-print function for Fb programs: an interpreter *)

let rep s = ppeval (parse s);;

(* 
let s1 = "Let f = Function x -> x + 1 In f 2" ;; 
*)
let s1 = "Let x = `Negative (0-3) In Match x With `Negative x -> x | `Zero x -> 0 | `Positive x -> x" ;;  

let s2 = "
Let ycomb = (Function body -> Let wrapper = Function this -> Function arg -> body (this this) arg In Function arg -> wrapper wrapper arg) In
ycomb (Function this -> Function x -> If x = 0 Then 0 Else x + this (x - 1)) 4
" ;;

let s3 = "
Let ycomb = (Function body -> Let wrapper = Function this -> Function arg -> body (this this) arg In Function arg -> wrapper wrapper arg) In
Let f = Function myaddr -> ycomb (
  Function this -> Function localdata -> Function msg ->
    Match msg With
      `main(n) -> (myaddr <- `count(n)); this(0)
    | `count(n) -> (Print n) ; (If n = 0 Then this(0) Else myaddr <- `count(n-1)); this(0)
)
In
Let a = Create(f, 0) In
  a <- `main(5)
" ;;

let s4 = "
Let ycomb = (Function body -> Let wrapper = Function this -> Function arg -> body (this this) arg In Function arg -> wrapper wrapper arg) In
Let f = Function myaddr -> ycomb (
  Function this -> Function localdata -> Function msg ->
    Match msg With
      | `count(n) -> (Print localdata) ; (If localdata = 0 Then this(0) Else myaddr <- `count(0)); this(localdata - 1)
)
In
Let a = Create(f, 5) In
  a <- `count(0)
" ;;

rep s4 ;;


let s7 = "
Let ycomb = (Function body -> Let wrapper = Function this -> Function arg -> body (this this) arg In Function arg -> wrapper wrapper arg) In
Let reversem = Function f -> Function this -> Function arg -> f arg this In
  ycomb (reversem (Function arg -> Function this ->
           If arg = 0 Then 0 Else arg + this (arg - 1))) 5
"
;;
rep s7 ;;

let s7 = "
Let ycomb = (Function body -> Let wrapper = Function this -> Function arg -> body (this this) arg In Function arg -> wrapper wrapper arg) In
Let count = ycomb (Function this -> Function data -> Function msg ->
  Match msg With 
    | `count(d) -> 
         Let dir = Fst data In Let c = Snd data In 
         Let newc = If dir Then c+1 Else c-1 In
           (Print newc); this (dir, newc)
    | `up(dx) -> (Print dx); this (dx, Snd data))
In
  Let f = Function myaddr -> Function data -> count (True, 0) 
In
  Let sendn = ycomb (Function this -> Function a -> Function n -> Function msg ->
    If n = 0 Then True Else (a <- msg) ; this a (n-1) msg)   
  In
  Let a = Create(f, 0) In
      sendn a 3 (`count 0) ; ( a <- `up(False) ) ; sendn a 3 (`count 0) 
  ";;
rep s7 ;;

  
  
