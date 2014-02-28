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

Name                  :
List of team Members  :
List of discussants   :

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
Problem 1b. (5 points)

Pairs were defined in the book as an encoding, but we can build them into the language.  Let's create
a language FbP: Fb with pairs.  Describe the changes to the expression and value grammar you will need;
then, write the operational semantics rule(s) you need to evaluate pairs.
*)

(* ANSWER *)


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

let fbIntCompare = ();; (* ANSWER *)

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

let fbMultiply = ();; (* ANSWER *)
let fbDivide = ();; (* ANSWER *)
let fbMod = ();; (* ANSWER *)

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

let fbUnchurch = ();; (* ANSWER *)
let fbChurch = ();; (* ANSWER *)

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

    n (function z -> a) (b)
    
This way, any Church zero will just return its argument (b, the false branch)
and any positive Church numeral will return some number of compositions over the
function.  Since the function throws away its input and just returns a (the
true branch), non-zero numbers will always return a.  :)
*)

let fbChurchFact = ();; (* ANSWER *)

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

let fbChurchAnd = ();; (* ANSWER *)
let fbChurchOr = ();; (* ANSWER *)
let fbChurchNot = ();; (* ANSWER *)

