(*
    600.426 - Programming Languages
    JHU Spring 2014
    Homework 3 Part 2 (35 points)

    Now that you have completed your Fb interpreter, you will need to write some
    programs in Fb.

    Note: We will be testing your Fb code with the canonical interpreter (binary) that was
    provided to you. So it is worth your while to test your code against that prior to submitting
    it.
*)

(* -------------------------------------------------------------------------------------------------- *)
(* HEADER: PLEASE FILL THIS IN                                                                        *)
(* -------------------------------------------------------------------------------------------------- *)

(*

Name                  : Fenghuan Lu
List of team Members  : N/A
List of discussants   : N/A

*)

(* -------------------------------------------------------------------------------------------------- *)
(* Section 1 : Operational Semantics and Proofs                                                       *)
(* -------------------------------------------------------------------------------------------------- *)

(*
Problem 1a. (5 points)

The operational semantics for Fb provide a set of proof rules which are used by the interpreter to
perform evaluation.  In this problem, you will build a proof by hand.  Write an operational semantics
proof which demonstrates that the Fb expression "(Function x -> x + 1) (If True Then 2 Else 4)"
evaluates to 3.
*)

(* ANSWER *)
(*                           
                        2 => 2 1 => 1
                            2 + 1 => 2
 -------------------------------------
 (Function x -> x + 1) 2 => x[2/x] + 1
                          True => True  2 => 2    
                          ------------ --------
  (Function x -> x + 1), If True Then 2 Else 4 => 2,  (x + 1)[2/x]
  ---------------------------------------------------
  (Function x -> x + 1) (If True Then 2 Else 4) => 3
 
*)
(*
Problem 1b. (5 points)

Pairs were defined in the book as an encoding, but we can build them into the language.  Let's create
a language FbP: Fb with pairs.  Describe the changes to the expression and value grammar you will need;
then, write the operational semantics rule(s) you need to evaluate pairs.
*)

(* ANSWER *)
(*
  In the fbast.ml file, change type expr to the following
  type expr =
      ... (* ast already defined *)
  | Pair of expr * expr | Left of expr | Right of expr

  Pair Rule
  =========
  Pair(e1, e2) => Pair(e1, e2)

  Left Rule
  ========
  e1 => v
  ---------
  e => Pair(e1, e2), Left e => v
  ------------------------------
  Left e => v

  Right Rule
  =========
  e1 => v                   
  ------------------
  e => Pair(e1, e2), Right e => v
  -------------------------------
  Right e => v
*)



(* -------------------------------------------------------------------------------------------------- *)
(* Fb utilities                                                                                       *)
(* -------------------------------------------------------------------------------------------------- *)
let mk_var ident = Var(Ident ident);;

let mk_function arg body =  Function(Ident arg, body);;

let mk_add arg1 arg2 = Plus(arg1, arg2);;

let mk_minus_var var1 var2 = Minus(mk_var var1, mk_var var2);;

let mk_minus arg1 arg2 = Minus(arg1, arg2);;

let mk_equal_var var1 var2 = Equal(mk_var var1, mk_var var2);;

let mk_equal arg1 arg2 = Equal(arg1, arg2);;

let mk_appl arg1 arg2 = Appl(arg1, arg2);;

let mk_appl_with_var var1 var2 = mk_appl (mk_var var1) (mk_var var2);;

let mk_if condi true_expr false_expr = If(condi, true_expr, false_expr);;

let mk_int v = Int(v);;


(*
   Function body ->
       Let wrapper = Function this -> Function arg -> body (this this) arg
       In Function arg -> wrapper wrapper arg
*)
let mk_combY =
  let this = "this" in
  let arg = "arg" in
  let body = "body" in
  (* Let wrapper = Function this -> Function arg -> body (this this) arg *)
  let wrapper = 
    mk_function this @: mk_function arg @: 
     mk_appl 
        (mk_appl (mk_var body) (mk_appl_with_var this this)) 
        (mk_var arg)
  in
  mk_function body  @: mk_function arg @: 
  mk_appl (mk_appl wrapper wrapper) (mk_var arg)
;;



(* -------------------------------------------------------------------------------------------------- *)
(* Section 2 : (En)Coding in Fb                                                                      *)
(* -------------------------------------------------------------------------------------------------- *)

(*
    The answers for this section must be in the form of Fb ASTs. You may assume that
    "fbdktoploop.ml" has been loaded before this code is executed; thus, you may use
    the parse function to create your answer if you like. Alternately you can create
    ASTs directly.

    For instance, the two definitions of the identity function in Fb are equivalent. (See below)
    The second one directly declares the datastructure produced by the first expression.

    You may use whichever form you please; the parse form is somewhat more readable, but
    the AST form allows you to create and reuse subtrees by declaring OCaml variables.
*)

let fb_identity_parsed = parse "Function x -> x";;

let fb_identity_ast = Function(Ident("x"), Var(Ident("x")));;


(*
Problem 2a. (4 points)

The following problem will get you used to writing Fb progrms.  We observe that
Fb is missing several features that one would normally take for granted in a
programming language.  For instance, we have integers but we have no way of
comparing them to determine which is greater.  We could add a concept of integer
comparison to the language semantics, but this would increase complexity.

Instead, write an Fb function which takes two integers.  It should return -1 if
the first integer is less than the second, 1 if the first integer is greater
than the second, and 0 if the two integers are equal.  (Remember that
fbIntCompare should not be an OCaml function; it should be an OCaml value
containing an Fb AST representing an Fb function.  Do not add any parameters to
this declaration.)

NOTE: Even if you implemented Let Rec in your Fb interpreter, you are not
permitted to use it here.  The recursion you use must be purely based on
functions alone.  Feel free to implement an Fb Y-combinator here.  For examples
and hints, see the file "src/Fb/fbexamples.ml" in the FbDK project.
*)

let fbIntCompare = 
  let f = parse "Function this -> Function arg -> Function x -> Function y -> (
                    If (x = y) Then 0
                    Else (
                      If (x - y) = arg Then 1 
                      Else (
                        If (x - y) = (0 - arg) Then (0 - 1) 
                        Else (this (arg + 1) x y) )
                    )
                  )"
  in
  mk_appl
   (mk_appl mk_combY f) @:
   mk_int 0
;; (* ANSWER *)

(*
# ppeval (Appl(Appl(fbIntCompare,Int(3)),Int(5)));;
==> -1
- : unit = ()
# ppeval (Appl(Appl(fbIntCompare,Int(7)),Int(0)));;
==> 1
- : unit = ()
# ppeval (Appl(Appl(fbIntCompare,Int(1)),Int(1)));;
==> 0
- : unit = ()
*)

(*
Problem 2b. (7 points)

Fb also fails to provide any operations over integers more complex than addition
and subtraction.  Below, define the following operations: multiplication,
integer division, and integer modulus.  (Hint: if you get stuck, try getting
them working for positive numbers first and then dealing with negatives.)  Your
division and modulus functions should diverge when the divisor is zero.
*)

let fbMultiply = 
  let f = parse "Function this -> Function r -> Function cmp -> Function count -> Function x -> Function y -> (
                    If x = 0 Then 0
                    Else (
                      If count = y Or count = 0 - y Then 
                          (If cmp y 0 = 1
                              Then r
                           Else 0 - r)
                      Else  this ( r + x ) cmp (count + 1) x y
                    )
                  )"
  in
  (* combY f 0 fbIntCompare 0 *)
  mk_appl(
    mk_appl
      (mk_appl
        (mk_appl mk_combY f) @:
        mk_int 0
      )   
      fbIntCompare
    ) @:
    mk_int 0
;; 

let fbDivide = 
  (* 
   * f = Function this r cmp x y -> 
   *                if x = 0 Or abs(x) < abs(y) then r
   *                else 
   *                this (r + If x y different sign Then -1 Else 1) 
   *                     cmp 
   *                     (x + If x y different sign Then y Else -y)
   * *)
  let f = parse "Function this -> Function r -> Function cmp -> Function x -> Function y -> (
                   If x = 0  Or 
                      (cmp 
                        (If cmp x 0 = 1 Then x Else 0 - x) 
                        (If cmp y 0 = 1 Then y Else 0 - y )
                      ) = 0 - 1
                        Then r
                   Else  this 
                            ( r + If (cmp x 0) + (cmp y 0) = 0 Then 0 - 1 Else 1 ) 
                            cmp 
                            (x + If(cmp x 0) + (cmp y 0) = 0 Then y Else 0 - y)
                            y
                  )"
  in
  (* combY f 0 fbIntCompare*)
  mk_appl
    (mk_appl
        (mk_appl mk_combY f) @:
        mk_int 0
    )   
    fbIntCompare
;; 

let fbMod = 
  (* 
   * f = Function this cmp x y -> 
   *                if x = 0 Or abs(x) < abs(y) 
   *                  Then If x y different sign Then x + y Else x
   *                else 
   *                this 
   *                     cmp 
   *                     (x + If x y different sign Then y Else -y)
   *                     y
   * *)
  let f = parse "Function this -> Function cmp -> Function x -> Function y -> (
                   If x = 0  Or 
                      (cmp 
                        (If cmp x 0 = 1 Then x Else 0 - x) 
                        (If cmp y 0 = 1 Then y Else 0 - y )
                      ) = 0 - 1
                        Then If cmp x 0 + cmp y 0 = 0 Then x + y Else x
                   Else  this 
                            cmp 
                            (x + If(cmp x 0) + (cmp y 0) = 0 Then y Else 0 - y)
                            y
                  )"
  in
  (* combY f 0 fbIntCompare*)
  mk_appl
    (mk_appl mk_combY f) 
    fbIntCompare
;; 


(*
# ppeval (Appl(Appl(fbMultiply,Int(3)),Int(5)));;
==> 15
- : unit = ()
# ppeval (Appl(Appl(fbMultiply,Int(0)),Int(2)));;
==> 0
- : unit = ()
# ppeval (Appl(Appl(fbMultiply,Int(-3)),Int(5)));;
==> -15
- : unit = ()
# ppeval (Appl(Appl(fbMultiply,Int(-2)),Int(-4)));;
==> 8
- : unit = ()
# ppeval (Appl(Appl(fbMultiply,Int(12)),Int(7)));;
==> 84
- : unit = ()

# ppeval (Appl(Appl(fbDivide,Int(12)),Int(4)));;
==> 3
- : unit = ()
# ppeval (Appl(Appl(fbDivide,Int(-8)),Int(2)));;
==> -4
- : unit = ()
# ppeval (Appl(Appl(fbDivide,Int(7)),Int(3)));;
==> 2
- : unit = ()

# ppeval (Appl(Appl(fbMod,Int(12)),Int(4)));;
==> 0
- : unit = ()
# ppeval (Appl(Appl(fbMod,Int(7)),Int(3)));;
==> 1
- : unit = ()
# ppeval (Appl(Appl(fbMod,Int(-9)),Int(4)));;
==> 3
- : unit = ()
*)

(*
Problem 2c. (3 points)

So far, we have informally demonstrated that even those things we normally take
for granted (such as integer division) can be encoded using functions.  Now, we
will demonstrate that we can go even further, replacing those constructs we
created for Fb with functional logic.

For this next problem, you will find the following link helpful:
http://en.wikipedia.org/wiki/Church_encoding .  This Wikipedia article describes
a technique known as "Church encoding".  In this problem, we will use Church
encoding to replace Fb integers!

Church encoding allows us to represent integers as functions.  For instance,
consider the following table:

    0 --> Function f -> Function x -> x
    1 --> Function f -> Function x -> f x
    2 --> Function f -> Function x -> f (f x)
    3 --> Function f -> Function x -> f (f (f x))
    ...

Effectively, the Church encoding of an integer N is a function which will
compose another function with itself N times.  Write some Fb functions which
will convert between the Fb native representation of an integer and the Church
encoding of an integer.
*)

let fbUnchurch = parse "Function church -> church (Function sum -> sum + 1) 0";;
let fbChurch = 
  let f = parse "Function this -> Function n -> 
                    If n = 0 Then x Else f (this (n-1) )"
  in
  mk_function "n" (
    mk_function "f" (
      mk_function "x" @: mk_appl (mk_appl mk_combY f) (mk_var "n")
    )
  )
;; 

(*
# let church2 = parse "Function f -> Function x -> f (f x)";;
val church2 : Fbast.expr =
  Function (Ident "f",
   Function (Ident "x",
    Appl (Var (Ident "f"), Appl (Var (Ident "f"), Var (Ident "x")))))
# ppeval (Appl(fbUnchurch,church2));;
==> 2
- : unit = ()
# ppeval (Appl(fbUnchurch,Appl(fbChurch,Int(7))));;
==> 7
- : unit = ()
# ppeval (Appl(Appl(Appl(fbChurch,Int(4)),(parse "Function n -> n + n")),Int(3)));;
==> 48
- : unit = ()
*)

(*
Problem 2d. (7 points)

Numbers aren't much use unless you can make them interact.  For instance, the
following operations work on Church number encodings.  The Wikipedia article
explains these operations, but they are presented here as usable Fb ASTs.
*)

let fbChurchAdd = parse "Function m -> Function n -> Function f -> Function x -> m f (n f x)";;
let fbChurchSucc = parse "Function n -> Function f -> Function x -> f (n f x)";;
let fbChurchMult = parse "Function m -> Function n -> Function f -> Function x -> m (n f) x";;
let fbChurchPred = parse "Function n -> Function f -> Function x -> n (Function g -> Function h -> h (g f)) (Function u -> x) (Function u -> u)";;
let fbChurchSub = Function(Ident("m"),Function(Ident("n"),Appl(Appl(Var(Ident("n")),fbChurchPred),Var(Ident("m")))));;

(*
For instance, consider the following evaluation:

# ppeval (Appl(fbUnchurch,Appl(Appl(fbChurchAdd,church2),church2)));;
==> 4
- : unit = ()

Marvelous!  :)

Write a Church encoding of factorial.  It should take in a single Church-encoded
number and return its Church-encoded factorial.  This function may not use any
Fb operation other than function application; for instance, you are not allowed
to convert the Church numeral into an Fb integer, operate on it, and then
convert it back.

Hint: You will need to perform a test to see if a number is zero at some point.
The following trick works nicely with Church numerals.  Instead of

    if n != 0 then a else b

write
    0 --> Function f -> Function x -> x
    1 --> Function f -> Function x -> f x
    2 --> Function f -> Function x -> f (f x)
    3 --> Function f -> Function x -> f (f (f x))
    ...
    
    n (function z -> a) (b)
    
This way, any Church zero will just return its argument (b, the false branch)
and any positive Church numeral will return some number of compositions over the
function.  Since the function throws away its input and just returns a (the
true branch), non-zero numbers will always return a.  :)
*)

let fbChurchFact =
  let church_one = parse "Function f -> Function x -> f x" in
  let church_if_zero = parse "Function n -> Function a -> Function b -> 
                                   n (Function z -> a) b"
  in
  let f = mk_function "this" (
              mk_function "n" @:
                (* church_if_zero n (Funcion w -> (n * this(n-1)) w ) church_one *)
                mk_appl(
                  mk_appl 
                    (mk_appl church_if_zero @: mk_var "n") 
                      (* Function w -> (n * this (n-1) w) *)
                      (mk_function "w" 
                        (mk_appl
                          (*  n*(this (n-1))  *)
                          (mk_appl
                              (mk_appl fbChurchMult @: mk_var "n")
                              (* this (n-1) *)
                              (mk_appl 
                                (mk_var "this") 
                                (mk_appl fbChurchPred @: mk_var "n")
                              )
                          )
                          (mk_var "w")
                        )
                      )
                )
                church_one 
  )
  in
  mk_appl mk_combY f
;; (* ANSWER *)

(*
# ppeval (Appl(fbUnchurch,Appl(fbChurchFact,Appl(fbChurch,Int(3)))));;
==> 6
- : unit = ()
# ppeval (Appl(fbUnchurch,Appl(fbChurchFact,Appl(fbChurch,Int(4)))));;
==> 24
- : unit = ()
# ppeval (Appl(fbUnchurch,Appl(fbChurchFact,Appl(fbChurch,Int(5)))));;
==> 120
- : unit = ()
*)

(*
Problem 2e. (4 points)

We've shown that integers can be encoded as functions.  How about booleans?
The following definitions represent the Church encoding of true and false.
*)

let fbChurchTrue = parse "Function t -> Function f -> t";;
let fbChurchFalse = parse "Function t -> Function f -> f";;

(*
These operations work in a fashion similar to pairing.  When we would normally
write "if b then x else y", we instead write "b x y".  If b is true, it will
return its first argument - the expression we want to use when the condition is
true.  If b is false, it will return its second argument.

Using this notion of boolean encoding, define Fb functions to represent the
standard boolean operators and, or, and not.  (You can find definitions of
these operations in lambda calculus format on the Wikipedia page.)
*)

let fbChurchAnd = parse "Function p -> Function q -> p q p";;
let fbChurchOr = parse "Function p -> Function q -> p p q";;
let fbChurchNot = parse "Function p -> Function t -> Function f -> p f t";;

