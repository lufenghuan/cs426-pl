(*  OCaml I - Examples *)

3 + 4;; (* use ;; to end input.  Notice how  types are INFERRED *)
let x = 3 + 4;; (* x forever more is 7 *)
x + 5;;
let y = 5 in x + y;; (* y is local when let .. in .. is used *)
(* y + 6 ;; *) (* errors because y was only defined locally in previous line *)

(* Boolean operations *)
true && false;;
true || false;;
1 = 2;; (* = not == for boolean comparison *)
1 <> 2;;

(* Int/Float non-overloading *)
1;;
1.0;; 
4 * 5;;
(* 4.0 * 1.5;; *) (* error - '*' operator is only for 'integers' *)
4.0 *. 1.5;; (* this works -- '*.' is for floats *)

(* everything in caml returns values (i.e. is an 'expression') - no commands *)
if (x = 3) then (5 + 35) else 6;;
(if (x = 3) then 5 else 6) * 2;; (* two branches of 'if' must have SAME type *)
(* (if (x = 3) then 5.4 else 6) * 2;; *) (* type error *)

(* Lists are easy to create and manipulate *)
[1; 2; 3];;
[1; 1+1; 1+1+1];;
["a"; "b"; "c"];;
(* [1; "a"];; *) (* error - all elements must have same type - HOMOGENEOUS *)
[];; (* empty list *)

(* Operations on lists.  Lists are represented as BINARY TREES with left child a leaf. *)
0 :: [1; 2; 3];; (* 'consing' an element to the front of a list - fast *)
[1; 2; 3] @ [4; 5];; (* appending lists - slower *)
let x = [2; 4; 6];;
let y = 0 :: x;;
x;; (* NOTICE: did not mutate list x by putting 0 on front, its still [2; 4; 6] *)

(* ====================================================================== *)

(*  OCaml II  - Examples *)


(* Tuples - fixed length lists, but types of each element CAN differ, unlike lists *)

(2, "hi");;
let tuple = (2, "hi");;

(* Functions *)

let squared x = x * x;; (* declare a function: "f" is its name, "x" is its one parameter.  return implicit.  *)
squared 4;; (* call a function -- separate arguments with S P A C E S *)

(*
 * - no return statement; value of the whole body-expression is what gets 
 *   returned
 * - type is printed as domain -> range
 * - "officially", Caml functions take only one argument - ! 
 *      multiple arguments can be encoded by some tricks (later)
*)

(* fibonacci series - 1 1 2 3 5 8 13 ... *)
let rec fib n =     (* the "rec" keyword needs to be added to allow recursion (ugh) *)
  if n <= 2 then
    1 
  else
    fib (n - 1) + fib (n - 2);;

fib 10;;

(*
 * - immutable programming: for/while loops are useless (they either never loop 
 *   or forever since the test is the same), so recursion is the only means of
 *   iteration
*)

(* anonymous functions: define a function as an expression *)
let funny = function x -> x + 1;; (* "x" is argument here -- can do one-argument functions only *)
((function x -> x + 1) 4) + 7;; (* such a function is an expression and can be used anywhere *)

(* 
 * functions can be passed to and returned from functions --> HIGHER-ORDER
 *)

(* multiple arguments - just leave  spaces between multiple arguments - this is "Currying" *)
let add x y = x + y;;
add 3 4;;
(add 3) 4;; (* same meaning as previous *)
let add3 = add 3;; (* don't need to give all arguments at once -- 'add3' is now a function like 'function y -> 3 + y' *)
add3 4;;
add3 20;;

(*
 * Observe 'int -> int -> int' is parenthesized as 'int -> (int -> int)' -- RIGHT associativity
 *)

(*** Patterns ***)


(* Basic pattern match with numbers *)

"Hawaii " ^ (match 5 with
	0 -> "Zero"
	| 5 -> "Five"  (* notice the "|" separator between multiple patterns *)
	| _ -> "Nothing") ^ "-O";; (* default case -- _ is a pattern matching anything and without a name for it *)

let mixemup n =
	match n with
		0 -> 4
	| 5 -> 0
	| x -> x + 1;; (* default case giving a name to the matched number, x *)

mixemup 3;; (* matches last case and x becomes 3 *)

(* List matching *)

let dum = [3;5;9];;  (* Note this list notation is shorthand for 3 :: ( 5 :: (9 :: [])) *)

match dum with
	[] -> []
| hd :: tl -> tl
;;

let getTail l = 
  match l with
    [] -> []
  | head :: tail -> tail 
;;
getTail [3;5;9] ;;

(* first successful match is taken *)
match ['h';'o'] with      (* recall ['h';'o'] is really 'h' :: ('o' :: []) *)
	x :: (y :: z) -> "first"
| x :: y -> "second"
| _ -> "third";;

match ["hi"] with (* ["hi"] is "hi" :: [] *)
	x :: y :: z -> "first"
| x :: y -> "second"
| _ -> "third";;

(* tuple - pattern matching *)
let tuple = (2, "hi", 1.2);;

match tuple with
  (f, s, th) -> s
;;

(* let can be combined with a single pattern match to assign multiple values *)
let mypair = (2.2, 'Z');;
let (f, s) = mypair;;

let getSecond t = 
  match t with
    (f, s) -> s
;;
getSecond (2, "hi");;

let getSecond (f,s) = s (* equivalent to the above *)
;;

(* warning - non-exhaustive pattern matching; avoid this *)
let getHead l = 
  match l with
    head :: tail -> head
;;

let test x = 
  match x with 4 -> 3 | 5 -> 5;;

(* getHead [];; *)  (* Caml warned before that this case is not covered *)


(* patterns in function definitions *)
let cadd p = match p with (x, y) -> x + y;;

let cadd (x, y) = x + y;; (* same result as the above - match on the argument directly (only works for 1-case matches) *)

(* This 'cadd' takes only a SINGLE argument - a tuple - like how C etc functions are called *)
cadd (1, 2);;

(* compare above add to this earlier one above, recalling: *)
let add x y = x + y;;
add 1 2;; (* notice different calling convention -- two ways to write the same function in Caml *)

let add = (+);; (* aside: this is how you can give any built-in infix operator a name as a function *)

(* using patterns in recursive functions: a function to reverse a list 
   
   We also want to study this function to show how its correctness is
   justified by an induction argument

*)

let rec rev l = 
  match l with
    [] -> []
  | x :: xs -> rev xs @ [x]
;;
rev [1;2;3];; (* = 1 :: ( 2 :: ( 3 :: [])) *)

(* Unwind this: the recursive calls to compute the above are: *)

(rev [2;3]) @ [1];;
(rev[3] @ [2]) @ [1];;
((rev[]@[3]) @ [2]) @ [1];;
(([]@[3]) @ [2]) @ [1];;

(* Theorem: rev reverses any list.
 * Proof: by induction on the length of the list, say l.
 * Case  l = []: obviously [] is correct
 * Case l is x :: xs for some x and xs:  we assume by induction that "rev xs" reverses the tail;
 * then, the result "rev xs @ [x]" will clearly be the reverse of the whole list.  QED. 
 *)


(* ====================================================================== *)

(*  OCaml III  - Examples *)

(* Immutable declarations *)
(*
 * All variable declarations in Caml are IMMUTABLE -- value will never change
 * helps in reasoning about programs, we know the variable's value is fixed
 *)
let y = 55 in let x = 5 in
  let f y = x + y in
    let x = 7 in  (* this is a SHADOWING (re-)definition of x, NOT an assignment *)
      (f y) + x (* x is 5 in the function (!), it was the value of x when f was defined *)
;;

(* top loop is conceptually an open-ended series of lets which never close: compare following with previous *)
let y = 55;;
let x = 5;;
let f y = x + y;;
let x = 7;; (* as in previous example, this is a nested definition, not assignment! *)
f y + x;;


(*
 * Function definitions are similar, you can't mutate an existing definition.
 * HINT: When interactively editing a group of functions that call each other, 
 *       re-submit ALL the functions to the top loop when you change any ONE 
 *       of them.  Otherwise you can have some functions using a now-shadowed version.
 *       One advantage of using ocamlc compiler is you will not have this problem.
 *)

let f x = x + 1;;

let g x = f (f x);;

(* lets change f, say we made an error in its definition above *)
let f x = if x <= 0 then 0 else x + 1;; 

g (-5);; (* we didn't recompile g and the version above still refers to now-shadowed f - !! *)

let g x = f (f x);; (* need to resubmit (identical) g code since it depends on f *)
g (-6);; (* now it works as expected *)

let rec ident l = match l with 
              [] -> []
            | hd :: tl ->  hd::(ident tl)
						
						(* mutually recursive functions must be defined together via the "and" keywd (yuck) *)
let rec 
     take l = match l with 
              [] -> []
            | hd :: tl ->  hd::(skip tl)
and
     skip l = match l with 
              [] -> []
            | x :: xs -> take xs;;

take [1;2;3;4;5;6;7;8;9;10];;
skip [1;2;3;4;5;6;7;8;9;10];;

(* here is a version that hides the skip function -- make both internal and export one *)
						
let justtake ll =
	let rec 
     ltake l = match l with 
              [] -> []
            | hd :: tl ->  hd::(lskip tl)
and
     lskip l = match l with 
              [] -> []
            | x :: xs -> ltake xs
		
in 
 ltake ll;;
			
justtake [1;2;3;4;5;6;7;8;9;10];;
						  
(* 
 * Higher Order Functions - 
 *          functions that either:
 *                        take other functions as arguments
 *                     or return functions as results
 *                     or both
 *
 *    Why?
 *       - "pluggable" programming by passing in and out chunks of code
 *       - greatly increases reusability of code since any varying part in
 *         a pattern can be pulled out as a function parameter
 *)

(* Lets show the power by extracting out some pluggable code *)

(* Example: multiply each element of a list by ten *)

let rec timestenlist l =
  match l with 
    []    -> []
  | hd::tl -> (hd * 10) :: timestenlist tl;;

timestenlist [3;2;50];;

(* Example: append gobble to a list of words *)


let rec appendgobblelist l =
  match l with 
    []    -> []
  | hd::tl -> (hd ^"gobble") :: appendgobblelist tl;;

appendgobblelist ["have";"a";"good";"day"];;
("have" ^"gobble") :: ("a"^"gobble") :: appendgobblelist ["good";"day"];;

(* Notice there is a *pattern* here of "do an operation on each list element"
 * So lets pull out the "times ten" / "add gobble" as a function parameter!
 * this is in fact a classic example, the map function *)

let rec map f l =
  match l with 
    []    -> []
  | hd::tl -> (f hd) :: map f tl;;

map (function x -> x * 10) [3;2;50];;
let middle = List.map (function s -> s^"gobble");;
middle ["have";"a";"good";"day"];;

(* This also shows why these "anonymous" functions are useful. *)

(* an equivalent way to do the same thing - give function a name first: *)
let f x = x * 10;;
map f [1;2;3;34;56;90];;

(* Composition function g o f - takes in any 2 functions and returns their composition
 *)
let compose g f = (function x -> g (f x));;
let compose g f x =  g (f x);;
let compose = (function g -> (function f -> (function x -> g(f x))));;
(* All three of the above definitions produce the same function *)

let plus3 x = x+3;;
let times2 x = x*2;;
let times2plus3 = compose plus3 times2;;
times2plus3 10;;
compose (function x -> x+3) (function x -> x*2) 10;; (* equivalent way *)
let composerest = compose (function x -> x+3);;
composerest (function x -> x*2) 10;;

(*
  Parametric and object polymorphism
*)

let id = function x -> x;;
(* id applied to int returns an int *)
id 3;;
(* SAME id applied to bool returns a bool *)
id true;;
(* conclusion: the type of id is PARAMETRIC, i.e. the return type is 
   parameterized by the type of the argument.  These are called Generic in Java.
	
	 We already saw many parametric functions, e.g. map above: *)

map (function x -> (float x) *. 10.0) [1;2;2;32];;

(*
     ML has parametric polymorphism -- the 'a -> 'a type of id is parametric:
	   what comes out is the *same* type as what came in. Generics is
	   another term for parametric polymorphism.

     Java has parametric polymorphism via generic types - same principle but
		 the types must be explicitly declared in Java and are inferred in ML.

     C++ Templates are fundamentally different under the hood; we will cover them later in course
*)


(*
 * Only 'let'-defined functions are polymorphic (yuck)
 *)

(* Background: turning a let-defined function into an argument to a function *)

let add1 = fun x -> x + 1;;
add1 4;;

(* Equivalent way to do the above: define add1 as an anonymous function and pass in *)

(function addup -> addup 4)(fun x -> x + 1);;

(* Lets try to do the same refactoring for a polymorphic function *)

let id = function x -> x;;

match id (true) with 
    true -> id (3) 
  | false -> id(4);;


(* The below will error - variable mono_id is not defined by let so it can't be polymorphic

(function mono_id -> 
	match mono_id(true) with 
                true -> mono_id(3) 
              | false -> mono_id(4)) (fun x -> x);;
*)

(* If only used at one type its OK: *)

(function mono_id -> mono_id 4) id;; (* mono_id is solely of type int -> int, thats OK *)


(* ******************************************************************* *)

(* An old PL Homework 1  - lets work through some of it to get some experience  
   with writing simple functional Caml programs *)

(* 
   1. Write a function 'compose_funs' which takes a list of functions 
   [f1; ...; fn] and returns a function representing the composition 
   fn o .. o f1 of all of these.
 *)

let rec compose_funs lf = 
  match lf with
    [] -> (function x -> x)
  | f :: fs -> (function x -> (compose_funs fs) (f x))
;;

(* equivalent alternate version - refactor to hoist out the "function x" *)

let rec compose_funs lf = 
  function x -> 
    (match lf with
      [] -> x
    | f :: fs -> (compose_funs fs) (f x)
    )
;;

(* Yet another equivalent alternative - hoist x up one more level *)

let rec compose_funs lf x = 
    match lf with
      [] -> x
    | f :: fs -> (compose_funs fs) (f x)
;;

(* tests *)

let composeexample = compose_funs [(function x -> x+1); (function x -> x-1);
				 (function x -> x*3); (function x -> x-1)];;
 
composeexample 5;; (* should return 14 *)


(*
   2. Write a function 'toUpperCase' which takes a list (l) of characters and 
   returns a list which has the same characters as l, but capitalized (if not
   already).
   
   Note: 1. Assume that the capital of characters other than alphabets 
            (A - Z or a - z), are the characters themselves e.g.

                character               corresponding capital character
 
                    a                             A
                    z                             Z
                    A                             A
                    1                             1
                    %                             %

         2. You can only use Char.code and Char.chr library functions.
            You CANNOT use Char.uppercase.
 *)

let toUpperChar c =
  let c_code = Char.code c in
  if c_code >= 97 && c_code <= 122 then
    Char.chr (c_code - 32) 
  else c;;
	

let rec toUpperCase l =
  match l with
    [] -> []
  | c :: cs -> toUpperChar c :: toUpperCase cs 
;;


(* test *)
toUpperCase ['a'; 'q'; 'B'; 'Z'; ';'; '!'];;
                             (* should return ['A'; 'Q'; 'B'; 'Z'; ';'; '!'] *)

(* could have used map instead (note map is built in as List.map): *)

let toUpperCase l = List.map toUpperChar l ;;

(* could have also defined it even more simply - partly apply the Curried map : *)

let toUpperCase = map toUpperChar ;;

(*
   3. Write a function 'nth' which takes a list (l) and index (n) and returns
   the nth element of the list. If n is an invalid index i.e. n is negative or
   l has less then (n + 1) elements raise 'exception Failure' declared below.
   
   Note: indices start with 0 for the head of the list, 1 for the next element
         and so on (similar to arrays).
 *)
			     
exception Failure;; (* declares an exception; just write "raise Failure" *)
                    (* to raise it *)

let rec nth l n = 
  match l with
    [] -> raise Failure
  | x :: xs -> 
      if n = 0 then	
        x
      else
        nth xs (n - 1)
;;


(* tests *)
nth [1;2;3] 0;; (* should return 1 *)
nth [1;2;3] 1;; (* should return 2 *)
nth [1;2;3] (-1);; (* should raise exception *)
nth [1;2;3] 3;; (* should raise exception *)

(*
   4. Write a function 'partition' which takes a predicate (p) and a list
   (l) as arguments  and returns a tuple (l1, l2) such that l1 is the
   list of all the elements of l that satisfy the predicate p and l2
   is the list of all the elements of l that do NOT satisfy p. The
   order of the elements in the input list (l) should be preserved.
   
   Note: A predicate is any function which returns a boolean.
         e.g. let isPositive n = (n > 0);; 
 *)

let rec partition p l = 
  match l with
    [] -> ([], [])
  | x :: xs -> 
      let (l1, l2) = partition p xs in
      if p x then
	(x :: l1, l2)
      else
	(l1, (x :: l2))
;;


(* test *)
let isPositive n = n > 0;;
partition isPositive [1; -1; 2; -2; 3; -3];; (* should return             *)
					     (*	([1; 2;	3], [-1; -2; -3]) *)

(* 
   5. Write a function 'diff' which takes in two lists l1 and l2 and returns 
      a list containing all elements in l1 not in l2.

   Note: You will need to write another function 'contains x l' which checks 
         whether an element 'x' is contained in a list 'l' or not.
 *)

let rec contains x l = 
  match l with
    [] -> false
  | y :: ys -> x = y || contains x ys
;;

let rec diff l1 l2 = 
  match l1 with
    [] -> []
  | x :: xs -> 
      if contains x l2 then
	diff xs l2
      else
	x :: diff xs l2
;;


(* tests *)
contains 1 [1; 2; 3];; (* should return true *)
contains 5 [1; 2; 3];; (* should return false *)
diff [1;2;3] [3;4;5];; (* should return [1; 2] *)
diff [1;2]   [1;2;3];; (* should return [] *)



(* One topic left in higher-order functions ...
   Currying - by logician Haskell Curry
*)

(* First lets recall how functions allow incremental arguments to be passed *)
let addC x y = x + y;;
addC 1 2;; (* recall this is the same as '(addC 1) 2' *)
let tmp = addC 1 in tmp 2;; (* the partial application of arguments - result is a function *)

(* an identical way to define addC, clarifying what the above means: *)
let addC = function x -> (function y -> x + y);;
(* yet another identical way .. *)
let addC x = function y -> x + y;;
addC 1 2;; (* same result as above *)

(* Also recall the related  non-Curry'ing version: use a pair of arguments instead *)
let addNC p =
	match p with (x,y) -> x+y;;

(* recall this equivalent abbreviation *)
let addNC (x, y) = x + y;;


(* Notice how the type of the above differs from addC's type *)
addNC (3, 4);;
(* addNC 3;; *) (* error, need all or no arguments supplied *)

(*
 * Fact: these two approaches to a 2-argument function are isomorphic.
 *
 * We now define two cool higher-order functions: 
 *
 * curry   - takes in non-curry'ing 2-arg function and returns a curry'ing version 
 *
 * uncurry - takes in curry'ing 2-arg function and returns an non-curry'ing version 
 * Since we can then go back and forth between the two reps, they are
 *      ***ISOMORPHIC***
 *)
let curry fNC = function x -> function y -> fNC (x, y);;
let uncurry fC = function (x, y) -> fC x y;;

let newaddNC = uncurry addC;;
newaddNC (2,3);;
let newaddC  = curry   addNC;;
newaddC 2 3;;

let noop1 = curry (uncurry addC);; (* a no-op *)
let noop2 = uncurry (curry addNC);; (* another no-op - no-ops together show isomorphism *)

(* End Currying topic *)

(* Misc minor stuff *)

(* print_x for atomic types 'x', again no overloading in meaning here *)
print_string ("hi\n");;

(* you CAN also declare types, anywhere in fact *)
let add (x: float) (y: float) = x +. y;;
let add (x: int) (y: int) = (((x:int) + y) : int);;

(* type abbreviations via "type" *)
type intpair = int * int;;
let f (p : intpair) = match p with 
                      (l, r) -> l + r
;;
(2,3);; (* ocaml doesn't call this an intpair by default *)
f (2, 3);; (* can pass it to the function expecting an intpair due to type defn *)
((2,3):intpair);; (* can also explicitly tag data with its type *)

(* ====================================================================== *)

(*  OCaml IV  - Examples *)

(* ******************************************************************* *)

(* Next topic: user-defined data-structures -- variants & records *)

(* 
 * Variant Type Declaration 
 *    - related to union types in C: "this OR that OR theother"
 *    - BUT like list/tuple they are immutable data structures
 *    - each case of the union is identified by a name called 'constructor'
 *      which serves for both
 *           - constructing values of the variant type
 *           - inspecting them by pattern matching
 *      - constructors must start with Capital Letter to distinguish from variables
 *      - type declarations needed but once they are in place type inference on them works
 *)

(* 
 * variant type for doing mixed arithmetic (integers and floats) 
 *)

type number = Fixed of int | Floating of float;;  (* read "|" as "or", same as match *)

Fixed 5;;
Floating 4.0;;
(* note these look like functions but they are not -- you always need to give arg *)

let pullout x = 
	match x with 
	| Fixed n -> n 
	| Floating z -> int_of_float z;;

pullout (Fixed 5);;

(* arithmetic operations on 'number' variant type *)
let add_num n1 n2 =
   match (n1, n2) with    (* aside -- note use of temporary pair here to simult. match on two variables  *)
     (Fixed i1, Fixed i2) ->       Fixed  (i1       +  i2)
   | (Fixed i1,   Floating f2) ->  Floating(float i1 +. f2) (* need to coerce here *)
   | (Floating f1, Fixed i2)   ->  Floating(f1       +. float i2) (* ditto *)
   | (Floating f1, Floating f2) -> Floating(f1       +. f2)
;;

add_num (Fixed 123) (Floating 3.14159);;


(*
 * Enum types 
 *     - special case of variant types, 
 *       where all alternatives are constants:
 *)
type sign = Positive | Negative | Zero;;

let sign_int n = if n > 0 then Positive else Negative;;
sign_int (-6);;

(* Multiple data items in a construct?  Use product type *)

type complex = CZero | Nonzero of float * float;;

(* 
 * Recursive data structures - most common usage of variant types 
 *
 *  - recursive types can refer to themselves in their own definition
 * 
 * e.g. binary trees:
*)
type itree = Nothing | ANode of int * itree * itree;; 

ANode(4,Nothing,ANode(2,ANode(18,Nothing,Nothing),Nothing));;

(* Polymorphic version of above which allows any type at leaves: *)

type 'a btree = Empty | Node of 'a * 'a btree * 'a btree;;

(* example trees *)

let whack = Node("whack!",Empty, Empty);;
let bt = Node("fiddly ",
	        Node("backer ",
		       Empty,
		       Node("crack ",
			      Empty,
			      Empty)),
	        whack);;


(* Lists could have been built with a recursive type declaration *)

type 'a mylist = Nil | Cons of 'a * 'a mylist;;

let mylisteg = Cons(3,Cons(5,Cons(7,Nil)));; (* isomorphic to [3;5;7] *)

let rec double_list_elts ml = 
	match ml with
	| Nil -> Nil
	| Cons(mh,mt) -> Cons(mh * 2,double_list_elts mt);;

double_list_elts mylisteg;;

(*
 * Operations on binary trees are similar to how we have been doing lists
 *
 *)


let rec add_gobble_tree_elts bintree =
   match bintree with
     Empty -> Empty
   | Node(y, left, right) ->
       Node(y^"gobble",add_gobble_tree_elts left,add_gobble_tree_elts right)
;;

let gobtree = add_gobble_tree_elts bt;;


let rec walk_inorder bintree doer =
   match bintree with
     Empty -> ()
   | Node(y, left, right) ->
       walk_inorder left doer; doer y; walk_inorder right doer
;;

let mydoer = print_string;;

walk_inorder bt mydoer;;


let rec lookup x bintree =
   match bintree with
     Empty -> false
   | Node(y, left, right) ->
       if x = y then 
         true 
       else if x < y then 
         lookup x left 
       else 
         lookup x right
;;

lookup "whack!" bt;;
lookup "flack" bt;;

let rec insert x bintree =
   match bintree with
     Empty -> Node(x, Empty, Empty)
   | Node(y, left, right) ->
       if x <= y then 
         Node(y, insert x left, right)
       else 
         Node(y, left, insert x right)
;;

let goobt = insert "goober " bt;;
bt;; (* remember that OCaml data structures are by default immutable! *)
let gooobt = insert "slacker " goobt;;


(* END variants *)


(* 
 * Record Declarations 
 *   - like tuples but with labels on fields.
 *   - similar to the structs of C/C++.
 *   - types are declared just like variants.
 *   - can be used in pattern matches as well.
 *   - again the fields are immutable by default
 *)

(* record to represent rational numbers *)
type ratio = {num: int; denom: int};;

let q = {num = 53; denom = 6};; (* contrast with the malloc approach of C *)

q.num;;
q.denom;;

let rattoint r =
	match r with 
   {num = n; denom = d} -> n / d;;

let rattoint {num = n; denom = d}  =
	 n / d;;

let rattoint r  =
	 r.num / r.denom;;

rattoint q;;

let add_ratio r1 r2 = {num   = r1.num * r2.denom + r2.num * r1.denom;
		       denom = r1.denom * r2.denom};;
add_ratio {num = 1; denom = 3} {num = 2; denom = 5};;

(* Annoying shadowing issue: there is one global namespace of record labels - yuck! *)
type scale = {num: int; coeff: float};; (* shadowing ratio's label num *)
(* q.num;; *) (* error: no longer can access q's num label since scale now owns "num" *)
q.denom;; (* this is still OK, label not overdefined *)
function x -> x.num;; (* this is why there is only one version of num allowed - inference *)

(* Caml programmers often use tuples instead of records since the record name
 * issue is not handled satisfactorily *)

(* 
 * State 
 *   - variables in ML are NEVER directly mutable themselves; only (indirectly)
 *     mutable if they hold a
 *      - reference
 *      - mutable record
 *      - array
 *
 *   - Indirect mutability - variable itself can't change, 
 *                           but what it points to can.
 *   - items are immutable unless their mutability is explicitly declared
 *   
 *   - DON'T use state unless its really needed (you can't use it in HW1)
 *)

(* References *)
let x = ref 4;;
(* x + 1;; *) (* a type error ! *)

!x + 1;; (* need !x to get out the value; something like *x in C *)

x := 6;; (* assignment - x must be a ref cell *)
!x + 1;; (* Woo wee!!  Mutation finally!!!!  Ahhhh!!!  (says the  hacker) *)

(* 'a ref is really implemented by a mutable record with one field, contents: 
     'a ref abbreviates the type { mutable contents: 'a }
             -- the keyword mutable on a record field means it can mutate *)

let x = { contents = 4};; (* identical to x's definition above *)
x := 6;;
x.contents <- 6;;  (* same effect as previous line: update contents field *)

!x + 1;;
x.contents + 1;; (* same effect as previous line *)

(* mutable tree *)

type mitree = Nothing | ANode of int * mitree ref * mitree ref
;;

(*
 * - Only ref typed variables or mutable records may be assigned to
 * - The notion of immutable variables is one of the great strengths of ML.
 * - 'let' doesn't turn into a mutation operator with 'ref's
 *)

let x = ref 4;;
let f () = !x;;

x := 234;;
f();;

let x = ref 6;; (* a shadowing old previous x definition, not an assignment to x *)
f ();;

(* more on mutable records *)
type mutable_point = { mutable x: float; mutable y: float };;
let translate p dx dy = 
                p.x <- (p.x +. dx); (* observe use of ";" here to sequence effects *)
                p.y <- (p.y +. dy);;
let mypoint = { x = 0.0; y = 0.0 };;
translate mypoint 1.0 2.0;;
mypoint;;


(* Yes, we can even write a while loop now! *)
let x = ref 1;;
while !x < 10 do 
 (print_int !x; 
 print_string "\n"); 
 x := !x + 1
done;;


(*
 * Why is immutability good?
 *    - programmer can depend on the fact that something will never be mutated 
 *        when writing code
 * 
 * ML still lets you express mutation, but its extra so you only use it when 
 *   its really needed
 *
 * Haskell has a pure core which doesn't allow mutation to enter at all
 * Don't use mutation on any HW problem unless there is an explicit OK to do so.
 *)

(* 
 * Arrays 
 *   - fairly self-explanatory
 *   - have to be initialized before using
 *   - in general there is no such thing as "uninitialized" in Caml.
 *)

let arrhi = Array.make 100 "";; (* size and initial value are the params here *)
let arr = [| 4; 3; 2 |];; (* another way to make an array *)
arr.(0);; (* access (oddly enough) *)
arr.(0) <- 55;; (* update *)
arr;;


(* Exceptions *)

exception Foo;;  (* This is a new form of top-level declaration, along with let, type *)

let f _ = raise Foo;; (* note no need to declare "raises Foo" here as in Java; no inference of it either *)
f ();;

exception Bar;;

let g _ = 
  try 
    f () 
  with 
    Foo ->  5 | Bar -> 3;;
g ();;

(* exceptions that pass up an argument *)
exception Goo of string;;
let f _ = raise (Goo "keyboard on fire");;
f ();;

let g () = 
  try 
    f () 
  with 
      Foo -> ()
		| Goo s ->
      (print_string("exception raised: ");
       print_string(s);print_string("\n"))
;;
g ();;

(* END Exceptions *)

(* ====================================================================== *)

(* OCaml V - Examples *)

(* Caml Modules - structures and functors *)

(* Modules in programming languages
   - a module is a larger level of program abstraction: functional units or library. 
   - e.g. Java package, C/C++ directory w/ header files
  
Some principles of modules:

  -  Modules have names they can be referenced by.
  -  A module itself contains declarations of functions, classes,  types, etc.
  -  The module has an interface in which it 
      * imports some things (e.g. other modules) from the outside and  
      * exports some things it has declared for outsiders to use; hides other things
  -  Module names may also be around at run-time (or not; depends on the language)
      this helps minimize name clashes across modules: com.bozo.Wank is different from com.fred.Wank

  -  Modules may support separate compilation: not all code is needed to compile
       Java: mostly but not completely supported (need all .class files on CLASSPATH)
       C/C++: yes (.h file is all that is needed for imports)
       Caml: yes in ocamlc mode (.mli interface file of modules used is all that is needed)


 The C/C++ module system
   - Informal use of files and filesystem directories as modules 
   - .h file declaring what is externally visible for a module

 Problems with C/C++ modules
   - There is a global space of function names, so there can be name clashes
   - There is no strict relation enforced between the .c and .h  files
       * bad programmers can write really ugly code
 
 The Java module system: packages

  - A cleaner version of the C/C++ spirit of module
  - Directory is explicitly a package; allows for nested packages
  - Implicit .h file in the public decls on classes/methods
  - Separate namespace for each package avoids name clashes
  - Have to have at least the .class files of the imports around to compile
  - Hard to read off publics - what is being exported from code  (JavaDoc helps this)

 Conclusion:  Java's modules are better than C/C++'s

*)

(* 
   Caml module definitions are called "structures" (struct keyword) 
    - collections of related definitions (functions, types, other structures, 
                                          exceptions, values, ...) given a name
    - The types of structs are called signatures (the sig keyword)
          - they are the interfaces for structures, and are something like Java interfaces
          - not all the items in the structure need to be in the signature: implicitly hides the missing things
					- structure contents can be regular values, functions, **types**, exceptions
 *)

module FSet =
struct
	
	exception NotFound
	
	type 'a set = 'a list (* sets are just lists but make a new type to keep them distinct *)
	
	let emptyset : 'a set = []
	
	let rec add x (s: 'a set) = ((x :: s) : ('a set))
	
	let rec remove x (s: 'a set) =
		match s with
			[] -> raise NotFound
		| hd :: tl ->
				if hd = x then
					(tl: 'a set)
				else
					hd :: remove x tl
	
	let rec contains x (s: 'a set) =
		match s with
			[] -> false
		| hd :: tl ->
				if x = hd then
					true
				else
					contains x tl
end
;;

(* observe what is printed in the top loop when the above is entered: a module *signature* is inferred *)

(* Using modules: its something like Java packages, qualify the name *)

let mySet = FSet.add 5 [];;
let myNextSet = FSet.add 22 mySet;;
FSet.contains 5 mySet;;
FSet.remove 5 myNextSet;;

open FSet;; (* puts an implicit "FSet." in front of all things in FSet; may shadow some names *)

add "a" ["b"];;
contains "a" ["a"; "b"];;


module type STUPIDSET = (* define a module type (signature) with no remove, probably a stupid idea *)
  sig
    exception NotFound
    type 'a set = 'a list
    val emptyset : 'a set
    val add : 'a -> 'a set -> 'a set
    val contains : 'a -> 'a set -> bool
  end
;;


module StupidSet = (FSet: STUPIDSET);; (* put a signature on a structure -- force it to have that sig *)

(* StupidSet.remove;; *) (* Error: remove not in signature! *)

FSet.remove;;  (* This is still fine, remember we are not mutating FSet when making StupidSet *)

(* Now lets do some non-stupid hiding.  Hiding types is possible and allows "black box" data structures 
   - good software engineering practice to enforce hiding of internals *)

module type  HIDDENSET= 
  sig
    type 'a set    (* hide the type 'a list here by not giving 'a set definition in signature *)
    val emptyset : 'a set
    val add: 'a -> 'a set -> 'a set
    val remove : 'a -> 'a set -> 'a set
    val contains: 'a -> 'a set -> bool    
  end
;;

module HiddenSet = (FSet: HIDDENSET);;

(* HiddenSet.add 3 [];; *) (* Error: [] not a set since we HID the fact that sets are really lists *)

let hs = HiddenSet.add 5 (HiddenSet.add 3 HiddenSet.emptyset);; (* now it works - <abstr> result means type is abstract *)
HiddenSet.contains 3 hs;;

(* Separate Compilation with Caml

    We can program Caml in a cc / javac like way - no top-loop
    Each module is in a separate file
    Syntax of module **body** is identical
    No header "module XX = struct .. end" 
    Name of module is capped name of file: fSet.ml defines module FSet 
    File fSet.mli holds the signature of module FSet
       if there is no file set.mli thats OK; you have nothing hidden
    main program that starts running is body of main.ml structure
    Use ocamlc to compile to an executable (or ocamlopt for native code)
    Also need to compile the .mli files! (unlike .h files)

Here is how the ocamlc compiler makes object files

      .ml --ocamlc--> .cmo
      .mli --ocamlc--> .cmi
*)

module FSett: sig (* contents of file fSett.mli *) end
          = struct (* contents of file fSett.ml *) end;;

module Main: sig (* contents of main.mli *) end
           = struct (* contents of main.ml *) end;;

(* See http://pl.cs.jhu.edu/pl/ocaml/code/sep.zip for the example we cover in lecture.
   see the ocaml manual Chapter 8 for the full documentation *)


(* 
   Functors
       A "function" from structures to structures
       Allows a module to be parameterized and so instantiated in multiple ways
			   - think of it as the ability to "plug in" a code module
			 In object-oriented languages, object polymorphism gives you much of this ability
			   - the "Animal" variable can have a Dog, Cat, Fish, etc plugged in to it
		   But, Caml has no object polymorphism and something is needed to support this
       General functors are found only in a few languages
*)


type comparison = Less | Equal | Greater
  
	(* here is a kind of struct that we can take as a parameter; in Java we would just use an interface Comparable *)
	
module type ORDERED_TYPE =
  sig
    type t
    val compare: t -> t -> comparison
  end;;

(* Here is a functor version of a set, you feed in a struct with the set element ordering defined on it *)

module FSetFunctor =
  functor (Elt: ORDERED_TYPE) ->
  struct
    type element = Elt.t
    type set = element list
    
    let empty = []

    let rec add x s =
      match s with
        [] -> [x]
      | hd::tl ->
          match Elt.compare x hd with
            Equal   -> s        
          | Less    -> x :: s   
          | Greater -> hd :: add x tl

    let rec contains x s =
      match s with
        [] -> false
      | hd::tl ->
          match Elt.compare x hd with
            Equal   -> true     
          | Less    -> false    
          | Greater -> contains x tl
  end;;

(* Here is a concrete ordering we can feed in, one over ints *)

module OrderedInt = 
  struct
    type t = int
    let compare x y = 
      if x = y then 
	Equal 
      else 
	if x < y then 
	  Less 
	else 
	  Greater
  end;;

(* Here is how we feed it in, instantiating the functor to give a structure *)

module OrderedIntSet = FSetFunctor(OrderedInt);; (* note how this looks like a function application *)

let myOrderedIntSet = OrderedIntSet.add 5 OrderedIntSet.empty;;
OrderedIntSet.contains 3 myOrderedIntSet;;

(* We can do the same thing for a string comparison *)

module OrderedString =
  struct
    type t = string
	  
    let compare x y = 
      if x = y then 
	Equal 
      else 
	if x < y then 
	  Less 
	else 
	  Greater
  end;;

module OrderedStringSet = FSetFunctor(OrderedString);; (* a DIFFERENT instantiation of same *)

let myOrderedStringSet = OrderedStringSet.add "abc" OrderedStringSet.empty;;


(* Functors also have signatures; there can also be type abstraction in a functor signature *)

module type SETFUNCTOR = (* below is the syntax for a signature of a functor *)
    functor (Elt: ORDERED_TYPE) ->
  sig
    type element = Elt.t      (* concrete *)
    type set                  (* abstract *)
    val empty : set
    val add : element -> set -> set
    val contains : element -> set -> bool
  end;;

module AbstractSet = (FSetFunctor : SETFUNCTOR);; (* slap that sig on a functor *)
module AbstractIntSet = AbstractSet(OrderedInt);;

AbstractIntSet.add 5 AbstractIntSet.empty;;

(* end functors topic *)


