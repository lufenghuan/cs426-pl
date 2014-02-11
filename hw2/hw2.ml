(*
  600.426 - Programming Languages
  JHU Spring 2014
  Homework 2

  In this source file, you will find a number of comments containing the text
  "ANSWER".  Each of these comments indicates a portion of the source code you
  must fill in yourself.  You are welcome to include other functions for use in
  your answers.  Read the instructions for each problem and supply a segment of
  code which accomplishes the indicated task.  For your convenience, a number of
  test expressions are provided for each problem as well as a description of
  their expected values.

  In this assignment, you *are* permitted to complete the listed tasks using any
  of the OCaml modules/functions.  However you are still required to avoid the use 
  of mutation unless explicitly specified in the question.
*)


(* ----------------------------------------------------------------------------------------- *)
(* Section 1 : The Game of Types                                                             *)
(* ----------------------------------------------------------------------------------------- *)

(*
  1. For the next several problems, you will be asked to produce an expression which
     has a given type.  It does not matter what expression you provide as long as it
     has that type; there may be numerous (or even infinite) answers for each
     question. Your answer may *not* produce a compiler warning.  You are *not*
     permitted to use explicit type annotations (such as "fun x:'a -> x").

     [20 Points]
*) 

(* Give an expression which has the following type: int ref list *)
let exp1 = ();; (* ANSWER *)

(* Give an expression which has the following type: 'a list -> bool *)
let exp2 = ();; (* ANSWER *)

(* Give an expression which has the following type: ('a -> 'b) -> a -> 'b *)
let exp3 = ();; (* ANSWER *)

(* Give an expression which has the following type: 'a list -> 'a list list -> 'a list *)
let exp4 = ();; (* ANSWER *)

(* Give an expression which has the following type: ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b *)
let exp5 = ();; (* ANSWER *)

(* Give an expression which has the following type: ('a -> 'a -> int) -> 'a list -> 'a list *)
let exp6 = ();; (* ANSWER *)

(* Give an expression which has the following type:  ('a -> 'a -> 'b) -> ('c -> 'a) -> 'c -> 'c -> 'b *)
let exp7 = ();; (* ANSWER *)

(* Give an expression which has the following type: unit -> 'a  *)
(* Hint: Trick question. Try staring at methods in Pervasives for a bit *)
let exp8 = ();; (* ANSWER *)

(* Give an expression which has the following type: ('a -> 'b) -> 'b -> 'a option -> 'b *)
let exp9 = ();; (* ANSWER *)

type ('a, 'b) sometype = Foo of 'a | Bar of 'b ;;

(* Give an expression which has the following type:
   ('a * 'b) list -> ('a -> 'b -> 'c) -> ('d, 'c) sometype list 
*)
let exp10 = ();; (* ANSWER *)

(* -------------------------------------------------------------------------------------------------- *)
(* Section 2 : Making Modules                                                                         *)
(* -------------------------------------------------------------------------------------------------- *)

(*
  2. There are various ways to represent a Set in a language. For this question, we will implement a Set
     module in terms of a "characteristic function" - i.e. a predicate that determines whether an element
     is a member of the set. 
    
     Sets defined this way often allow large (or even infinite) sets to be concisely represented. For example, 
     the predicate (fun x -> x > 0 && x mod 2 = 0) represent the set of even naturals, while a predicate 
     (fun x -> x > 0.0 && x < 1.0) represents the set of floating point numbers between 0 and 1.
     
     Create a set module that has the following methods:
       create    : ('a -> bool) -> 'a set       - Create a new set instance based on the given predicate
       member    : 'a set -> 'a -> bool         - Returns true if that given element is a member of the set
       union     : 'a set -> 'a set -> 'a set   - Returns a new set that is the union of two sets
       intersect : 'a set -> 'a set -> 'a set   - Returns a new set that is the intersection of two sets 
       exclude   : 'a set -> 'a -> 'a set       - Returns a new set that excludes the specific element 
      
     In the section below, fill out the signature and the implementation details. You must explicitly leave any
     types in the signature abstract. (This is good practice in the software engineering sense. By not explicitly
     binding the types on the interface, you allow different implementations to choose types best suited
     for their goals)
    
     NOTE: When you query the type of your functions in the top loop, it might return the fully qualified type signatures.
     E.g: create : ('a -> bool) -> 'a CFSet.set instead of just unit -> 'a set. This is fine.
    
     [15 Points]
*)

module type CFSETTYPE =
  sig
      (* ANSWER *)
  end
;;

module CFSet : CFSETTYPE =
  struct
      (* ANSWER *)         
  end
;;

(*
# let q1 = CFSet.create (fun x -> x mod 2 = 0) ;;
val q1 : int CFSet.set = <abstr>
# List.map (CFSet.member q1) [3; 4; 5; 10; 11; 15];;
- : bool list = [false; true; false; true; false; false]
# let q2 = CFSet.create (fun x -> x mod 5 = 0) ;;
val q2 : int CFSet.set = <abstr>
# List.map (CFSet.member q2) [3; 4; 5; 10; 11; 15];;
- : bool list = [false; false; true; true; false; true]
# let q3 = CFSet.union q1 q2 ;;
val q3 : int CFSet.set = <abstr>
# List.map (CFSet.member q3) [3; 4; 5; 10; 11; 15];;
- : bool list = [false; true; true; true; false; true]
# let q4 = CFSet.intersect q1 q2 ;;
val q4 : int CFSet.set = <abstr>
# List.map (CFSet.member q4) [3; 4; 5; 10; 11; 15; 20];;
- : bool list = [false; false; false; true; false; false; true]
# let q5 = CFSet.exclude q4 10 ;;
val q5 : int CFSet.set = <abstr>
#  List.map (CFSet.member q5) [3; 4; 5; 10; 11; 15; 20];;
- : bool list = [false; false; false; false; false; false; true]
*)

(* -------------------------------------------------------------------------------------------------- *)
(* Section 3 : Trees                                                                                  *)
(* -------------------------------------------------------------------------------------------------- *)

(* Non-empty Trees with a variable number of children can be represented by the following data structure *)

type 'a tree = Node of 'a * 'a tree list ;;

(* For example:
   Node('a', 
     [Node('b', 
        [Node('c', []);
         Node('d', [])]);
      Node('e',
        [Node('f',
           [Node('g', [])])])])
                      
represents the tree:

        a
      /   \
     b     e
   /  \    |
  c    d   f
           |
           g 
*)

(*
  3a. Instead of writing down the whole tree like the above, we can define an encoding of trees as a list of pairs.
      
      The first element of the pair is the data that is stored in the node, and the second is the number of 
      children. The child trees are listed immediately after their parent nodes. The tree above can be represented
      like so: [('a',2);('b',2);('c',0);('d',0);('e',1);('f',1);('g',0)] 
      
      Write a function to take an encoding of the above form and produce a tree instance.
      
      [10 Points]
*)

let rec decode_tree encoded_tree = () (* ANSWER *) ;;

(*
# decode_tree  [('a',2);('b',2);('c',0);('d',0);('e',1);('f',1);('g',0)] ;;
- : char tree =
Node ('a',
 [Node ('b', [Node ('c', []); Node ('d', [])]);
  Node ('e', [Node ('f', [Node ('g', [])])])])
# decode_tree 
   [("A",6);("B",3);("H",2);("N",0);("O",0);("I",0);("J",0);("C",0);("D",0);
    ("E",2);("K",1);("P",0);("L",1);("Q",0);("F",0);("G",1);("M",0)] ;;
- : string tree =
Node ("A",
 [Node ("B",
   [Node ("H", [Node ("N", []); Node ("O", [])]); Node ("I", []);
    Node ("J", [])]);
  Node ("C", []); Node ("D", []);
  Node ("E", [Node ("K", [Node ("P", [])]); Node ("L", [Node ("Q", [])])]);
  Node ("F", []); Node ("G", [Node ("M", [])])])
*)

(*
  3b. Now write a function to take a tree instance and produce its equivalent encoding.

      [5 Points]
*)

let rec encode_tree tree = () (* ANSWER *) ;;
    
(*
# encode_tree (Node ('a',
   [Node ('b', [Node ('c', []); Node ('d', [])]);
    Node ('e', [Node ('f', [Node ('g', [])])])])) ;;
- : (char * int) list = [('a', 2); ('b', 2); ('c', 0); ('d', 0); ('e', 1); ('f', 1); ('g', 0)]
# encode_tree 
  (Node ("A",
    [Node ("B",
      [Node ("H", [Node ("N", []); Node ("O", [])]); Node ("I", []);
       Node ("J", [])]);
     Node ("C", []); Node ("D", []);
     Node ("E", [Node ("K", [Node ("P", [])]); Node ("L", [Node ("Q", [])])]);
     Node ("F", []); Node ("G", [Node ("M", [])])]));;
- : (string * int) list =
[("A", 6); ("B", 3); ("H", 2); ("N", 0); ("O", 0); ("I", 0); ("J", 0);
 ("C", 0); ("D", 0); ("E", 2); ("K", 1); ("P", 0); ("L", 1); ("Q", 0);
 ("F", 0); ("G", 1); ("M", 0)]
*)

(*
  3c. Write a function that takes as input, a function (of type 'a -> 'a) and a tree and creates a new tree by 
      applying the function to each node.
      
      [5 Points]
*)

let rec map_tree fn tree = () (* ANSWER *) ;;

(*
# map_tree (fun x -> Char.code x - Char.code 'a') (Node ('a',
   [Node ('b', [Node ('c', []); Node ('d', [])]);
    Node ('e', [Node ('f', [Node ('g', [])])])])) ;;
- : int tree =
Node (0,
 [Node (1, [Node (2, []); Node (3, [])]);
  Node (4, [Node (5, [Node (6, [])])])])
# map_tree (fun x -> Char.chr (x + Char.code 'A' - 1)) ( decode_tree [(1,3);(2,0);(3,1);(4,1);(5,0);(6,0)] );;
- : char tree =
Node ('A',
 [Node ('B', []); Node ('C', [Node ('D', [Node ('E', [])])]); Node ('F', [])])
*)

(*
  3d. A common operation on trees is to walk the structure in some order and build up a return value. For 
      example, with a binary search tree, you can do an in-order walk to create a sorted list. Similarly it
      is often useful to do a post-order walk to consolidate information from child nodes in the tree.
      
      This is such a general concept that it is useful to write a combinator to help us.
  
      Given a binary function f with signature ('a -> 'b tree -> 'a), an initial result of type 'a and a tree of type 
      'b tree, write a function that walks the tree in postorder & left-to-right (i.e. Child nodes are processed 
      before parent nodes and in a left to right order) and builds a final result by repeatedly applying 'f' to the 
      previous result and the value of the current tree node to produce a new result. 
      
      [10 Points]
*)

let rec fold_tree_postorder fn init tree = () (* ANSWER *) ;;

(*
# fold_tree_postorder (function res -> function Node(a,_) -> res ^ (String.make 1 a)) "" 
  (Node ('a',
     [Node ('b', [Node ('c', []); Node ('d', [])]);
      Node ('e', [Node ('f', [Node ('g', [])])])])) ;;
- : string = "cdbgfea"
# let add_sum dict (Node((n, sz), children)) = 
    let child_values nodes = List.map (function Node((a,_), _) -> List.assoc a dict) nodes in
    let sum = List.fold_left (fun s -> function v -> s + v) sz (child_values children) in
      (n,sum)::dict ;;         
# fold_tree_postorder (function res -> fun v -> add_sum res v) []
  (Node (('A',8),
   [Node (('B',3), [Node (('C',1), []); Node (('D',6), [Node (('E',4), []); Node (('F',7), [])])]);
    Node (('G',10), [Node (('H',14), [Node (('I',13), [])])])])) ;;
- : (char * int) list =
[('A', 66); ('G', 37); ('H', 27); ('I', 13); ('B', 21); ('D', 17); ('F', 7);
 ('E', 4); ('C', 1)]
*)

(* -------------------------------------------------------------------------------------------------- *)
(* Section 4 : Relations                                                                              *)
(* -------------------------------------------------------------------------------------------------- *)

(* A finite binary relation can be represented with the following type *)

type ('a, 'b) binary_relation = ('a * 'b) list ;;


(*
  3a. A finite relation defined by the above type is symmetric if for every (x, y) in the relationship, (y, x) 
      is also in the relation.
  
      Write a function to determine whether the given relation is symmetric.
      
      [5 Points]
*) 

let rec is_symmetric rel = () (* ANSWER *) ;;

(*
# is_symmetric [(1,4);(1,2);(1,3);(3,2);(2,3);(4,1);(2,1);(3,1)] ;;
- : bool = true
# is_symmetric [(1,2);(1,3);(3,2);(2,3);(4,1);(2,1);(3,1)] ;;
- : bool = false
*)

(*
  3b. Similarly the finite relation from above is transitive if for every (x, y) and (y, z) in the relation (x, z) 
      is also in the relation
  
      Write a function to determine whether the given relation is transitive.
      
      [10 Points]
*)

let rec is_transitive rel = () (* ANSWER *) ;;

(*
# is_transitive [(1,2);(1,3);(2,3);(4,4);(1,4);(4,3);(2,4);(4,2);(2,2)] ;;
- : bool = true
# is_transitive [(1,2);(1,3);(4,4);(1,4);(4,3);(2,4);(4,2);(2,2)] ;;
- : bool = false
*)

(* -------------------------------------------------------------------------------------------------- *)
(* Section 5 : Mutable State and Memoization                                                          *)
(* -------------------------------------------------------------------------------------------------- *)

(* Note: You will need to use mutable state in some form for questions in this section *)

(*
  5a. Cache: Pure functions (those without side effects) always produces the same value
      when invoked with the same parameter. So instead of recomputing values each time,
      it is possible to cache the results to achieve some speedup.
     
      The general idea is to store the previous arguments the function was called
      on and its results. On a subsequent call if the same argument is passed, 
      the function is not invoked - instead, the result in the cache is immediately 
      returned.  
  
      [10 Points]
*)

(*
  Given any function f as an argument, create a function that returns a
  data structure consisting of f and its cache
*)  
let new_cached_fun f = () (* ANSWER *)

(*
  Write a function that takes the above function-cache data structure,
  applies an argument to it (using the cache if possible) and returns
  the result 
*)
let apply_fun_with_cache cached_fn x = () (* ANSWER *)

(*
  The following function makes a cached version for f that looks
  identical to f; users can't see that values are being cached 
*)

let make_cached_fun f = 
  let cf = new_cached_fun f in 
    function x -> apply_fun_with_cache cf x
;;


(*
let f x = x + 1;;
let cache_for_f = new_cached_fun f;;
apply_fun_with_cache cache_for_f 1;;
cache_for_f;;
apply_fun_with_cache cache_for_f 1;;
cache_for_f;;
apply_fun_with_cache cache_for_f 2;;
cache_for_f;;
apply_fun_with_cache cache_for_f 5;;
cache_for_f;;
let cf = make_cached_fun f;;
cf 4;;
cf 4;;


# val f : int -> int = <fun>
# val cache_for_f : ... 
# - : int = 2
# - : ...
# - : int = 2
# - : ...
# - : int = 3
# - : ...
# - : int = 6
# - : ...
# val cf : int -> int = <fun>
# - : int = 5
# - : int = 5
*)



(*
  5b. We have been writing a lot of data structure specific traversal code for these assignments. One of the
      ways we can abstract this a bit is to introduce the notion of iterators (like in Java). 
  
      Conceptually an iterator is just a *stateful* function that when invoked returns the "next" element of an 
      underlying container (for some notion of next).   

      We will experiment with a couple of iterators in this question.
      
      [10 Points]
*)

(*
  Given a list, create and return an iterator function for it. If the list has the type 'a list, the returned
  function should have the type (unit -> 'a option). Successive invocations of the function should return successive
  elements from the list. Invoking the function after all the elements are exhausted, should return None.
*)

(* 'a list -> (() -> 'a option) *)
let create_list_iterator lst = () (* ANSWER *) ;; 

(*
# let iter = create_list_iterator [3;2;1] ;;
val iter : unit -> int option = <fun>
# iter () ;;
- : int option = Some 3
# iter () ;;
- : int option = Some 2
# iter () ;;
- : int option = Some 1
# iter () ;;
- : int option = None
# iter () ;;
- : int option = None
*) 

(*
  In fact you don't even need an actual underlying datastructure. The above definition is general enough to admit 
  iterators over infinite sequences.
  
  For this question, given a starting point ('a') and a common difference ('d'), create a simple iterator for an 
  arithmetic progression - i.e. an iterator over the infinite sequence (a, a + d, a + 2d ..... )
*)

(* int -> int -> (() -> int option) *)
let create_ap_iterator a d = () (* ANSWER *) ;;

(*
# let iter = create_ap_iterator 2 5 ;;
val iter : unit -> int option = <fun>
# iter () ;;
- : int option = Some 2
# iter () ;;
- : int option = Some 7
# iter () ;;
- : int option = Some 12
*) 
