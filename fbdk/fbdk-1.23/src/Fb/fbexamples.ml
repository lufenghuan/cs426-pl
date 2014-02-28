(* These are the Fb examples from the book.  Requires fbdktoploop.ml *)

let ex2dot6 = parse "If Not(1 = 2) Then 3 Else 4";;

let ex2dot6 = If(Not(Equal(Int 1, Int 2)), Int 3, Int 4);; (* parsed form of above *)

let ex2dot7 = parse "(Function x -> x + 1) 5" ;;
let ex2dot7 = Appl(Function(Ident "x", Plus(Var(Ident "x"), Int 1)), Int 5) ;;

let ex2dot8 = parse "(Function x -> Function y -> x + y) 4 5" ;;
let ex2dot8 = Appl(Appl(Function(Ident "x", Function(Ident "y", 
  Plus(Var(Ident "x"), Var(Ident "y")))), Int 4), Int 5);;

let ex2dot9 = parse "Let Rec fib x =
    If x = 1 Or x = 2 Then 1 Else fib (x - 1) + fib (x - 2)
    In fib 6" ;;
let ex2dot9 = Letrec(Ident "fib", Ident "x", 
  If(Or(Equal(Var(Ident "x"), Int 1), 
        Equal(Var(Ident "x"), Int 2)), 
    Int 1, 
    Plus(Appl(Var(Ident "fib"), Minus(Var(Ident "x"), Int 1)), 
         Appl(Var(Ident "fib"), Minus(Var(Ident "x"), Int 2)))),
  Appl(Var(Ident "fib"), Int 6));;

let ex2dot3dot2 = parse "(Function x -> x + 2)(3 + 2 + 5)" ;;

let ex2dot3dot2b = parse "(Function x -> Function x -> x) 3" ;;

let ex2dot11 = parse "Function x -> Function y -> x + y + z";;

let ex2dot3dot2c = parse 
 "Let Rec x1 x2 = 
     If x2 = 1 Then
          (Function x3 -> x3 (x2 - 1)) (Function x4 -> x4)
     Else
          x1 (x2 - 1)
  In x1 100";;


let ex2dot3dot3 = parse "If 3 = 4 Then 5 Else 4 + 2" ;;
   
let ex2dot3dot3b = parse "(Function x -> If 3 = x Then 5 Else x + 2) 4 " ;;

let ex2dot3dot3c = parse "(Function x -> x x)(Function y -> y) " ;;

let ex2dot3dot3d = parse "(Function f -> Function x -> f(f(x)))
           (Function x -> x - 1) 4" ;;

let ex2dot3dot3e = parse "(Function x -> Function y -> x + y)
    ((Function x -> If 3 = x Then 5 Else x + 2) 4)
    ((Function f -> Function x -> f (f x))
            (Function x -> x - 1) 4 )" ;;

let ex2dot3dot3f = parse "Let Rec f x = 
    If x = 1 Then 1 Else x + f (x - 1)
In f 3" ;;


let ex2dot3dot3g = parse "Let Rec f x = 
    If x = 1 Then 1 Else x + f (x - 1)
  In f" ;;


let lemma2dot4 = parse "(Function x -> x x)(Function x -> x x)" ;;

let combI = parse "Function x -> x";;
let combK = parse "Function x -> Function y -> x";;
let combS = parse "Function x -> Function y -> Function z -> (x z) (y z)";;
let combD = parse "Function x -> x x";;


(* Tuples *)

let pr (e1,e2) = Appl(Appl(parse "(Function lft -> Function rgt -> Function x -> x lft rgt)",e1),e2);;

let left e = Appl(e,parse "(Function x -> Function y -> x)");;
let right e = Appl(e,parse "(Function x -> Function y -> y)");;

let p = pr(Int 4, Int 5);;

let ex2dot3dot4 = left p;;

(* Lists *)
let cons (e1, e2) = pr(pr(Bool false, e1), e2)
let emptylist = pr(pr(Bool true, Int 0),Int 0)
let head e = right (left e)
let tail e = right e
let isempty e = (left (left e))
let length = Letrec (Ident "len", Ident "x",
   If (isempty (Var (Ident "x")), Int 0,
    Plus (Int 1,
     Appl (Var (Ident "len"), tail(Var (Ident "x"))))),
   Var (Ident "len"))

let eglist = cons(Int 0,cons(Int 4,cons(Int 2,emptylist)));;
let eghd = head eglist;;
let egtl = tail eglist;;
let eghdtl = head (tail eglist);;
let eglength = Appl(length,eglist);;

(* Freeze and thaw macros *)
let freeze e = Function(Ident"x", e);;
let thaw e = Appl(e,Int 0);;

(* Recursion *)
let summate0 = parse "Function this -> Function arg ->
    If arg = 0 Then 0 Else arg + this this (arg - 1)";;

let summate0test = Appl(Appl(summate0, summate0), Int 7);;

(* Let definition is expanded in the below compared to book version *)
let summate =
  parse "(Function summ -> Function arg -> summ summ arg)
          (Function this -> Function arg ->
           If arg = 0 Then 0 Else arg + this this (arg - 1))";;

let summatetest = Appl(summate, Int 7);;

let almosty = parse "Function body -> 
    (Function fun ->  Function arg -> fun fun arg)
      (Function this -> Function arg ->
        body this arg)";;


let summate = Appl(almosty, parse "(Function this -> Function arg ->
    If arg = 0 Then 0 Else arg + this this (arg - 1))");;

let summatetest = Appl(summate, Int 7);;


let combY = parse "Function body -> 
    (Function fun -> Function arg -> fun fun arg)
      (Function this -> Function arg -> body (this this) arg)";;

let summate = Appl(combY, parse "Function this -> Function arg ->
    If arg = 0 Then 0 Else arg + this (arg - 1)");;

let summatetest = Appl(summate, Int 7);;
