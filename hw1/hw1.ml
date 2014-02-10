(*
  600.426 - Programming Languages
  JHU Spring 2014
  Homework 1
  
  In this source file, you will find a number of comments containing the text
  "ANSWER".  Each of these comments indicates a portion of the source code you
  must fill in yourself.  Read the instructions for each problem and supply a
  segment of code which accomplishes the indicated task.  For your convenience,
  a number of test expressions are provided for each problem as well as a
  description of their expected values.
  
  Please also fill in the HEADER section right below this one.
  
  Please note that you are NOT permitted to use any OCaml module functions (such
  as List.length) to complete this assignment. You are also not permitted to make 
  use of mutation (references, arrays, etc.). On the other hand you are encouraged 
  to write additional helper functions and reuse earlier functions in the file.
  
  Make sure to eliminate compiler/interpreter warnings before submitting your code.
  
*)

(* -------------------------------------------------------------------------------------------------- *)
(* HEADER: PLEASE FILL THIS IN                                                                        *)
(* -------------------------------------------------------------------------------------------------- *)

(*
 
  Name                        : Fenghuan Lu
  List of team Members        : N/A
  List of other collaborators : N/A

*) 

(* -------------------------------------------------------------------------------------------------- *)
(* List Utilities                                                                                     *)
(* -------------------------------------------------------------------------------------------------- *)


(* low precedence function application allows us to remove
 *  many () from our code 
 *  *)
let (@:) f x = f x;;

let list_length lst =
 let rec lst_length_helper lst len = 
   match lst with
      [] -> len
    | a::l -> lst_length_helper l (len + 1) 
 in
 lst_length_helper lst 0
;;

(* tail recursive map *)
let list_map f lst = 
  let rec map acc = function
    | [] -> acc
    | hd :: tl -> map ( (f hd) :: acc ) tl
  in
  map [] lst
;;

(* reverse list*)
let list_rev lst = 
  let rec rev lst rev_lst = 
    match lst with
      | [] -> rev_lst
      | hd :: tl -> rev tl (hd :: rev_lst)
  in
  rev lst []
;;

(* Split a list into two parts; the length of the first part is given *)
let list_split lst n =
  let rec split i acc = function
    | [] -> list_rev acc, []
    | h :: t as l -> if i = 0 then list_rev acc, l
                     else split (i-1) (h :: acc) t  
  in
  split n [] lst;;

(* flatten list of list *)
let rec list_flatten lst_lst = 
  match lst_lst with
    [] -> []
  | hd :: tl -> hd @ list_flatten tl
;;

let rec list_find p = function
  | [] -> raise Not_found
  | hd :: tl -> if p hd then hd else list_find p tl
;;


let rec list_contain x = function
  | [] -> false
  | h :: t -> compare x h = 0 || list_contain x t
;;


(* -------------------------------------------------------------------------------------------------- *)
(* Section 1 : Making and Breaking Lists                                                              *)
(* -------------------------------------------------------------------------------------------------- *)

(*
  1a. A predicate function is a function that accepts an argument and returns a boolean value. Given a
      list and a predicate, return the index of the first element in the list for which the predicate 
      evaluates to true. If no element in the list matches the predicate, return -1.
      
      [5 Points]
*) 

let rec index_of lst predicate = 
  match lst with
    [] -> -1
  | hd :: tl -> 
      if predicate hd  then 0 
      else 
        let idx = index_of tl predicate in
        if idx == -1 then -1
        else 1 + idx
;;

(*
# index_of [1;2;3;4;5;6] (fun x -> x > 3) ;; 
- : int = 3
# index_of [(1,1);(1,3);(2,2);(3,4)] (fun (x,y) -> x+y < 0) ;;
- : int = -1
*)

(*
  1b. Given a list and a number n, write a function that returns the last n elements of the list
      if n is greater than the length of the list, your function should return the entire list
      
      [5 Points]
*)

let rec back lst n = 
 match lst with 
   [] -> []
 | hd :: tl -> if list_length lst <= n then lst else back tl n

;; (* ANSWER *)
(*
# back [1;2;3;4;5;6;7;8] 4 ;;
- : int list = [5; 6; 7; 8]
# back ['a';'b';'c'] 4 ;;
- : char list = ['a'; 'b'; 'c']
*)


(*
  1c. Write a function that when given a list returns true if there are duplicate elements in
      the list.
      
      [5 Points]
*)

let rec has_duplicates lst = 
  match lst with
    [] -> false
  | hd :: tl -> if index_of tl (fun x -> x = hd ) > 0  then true else has_duplicates tl
;; (* ANSWER *)

(*
# has_duplicates [1;2;3;4;2;5] ;;
- : bool = true
# has_duplicates ["ab";"cd";"gh";"ij"] ;;
- : bool = false
*)

(*
  1d. The cartesian product of two sets S1 and S2 is the set of all pairs such that the first element of 
      each belongs to S1 and the second element belongs to S2.
      
      We don't have a data structure for sets (yet). So we will use lists instead. Accordingly we define 
      the cartesion product of two lists lst_1 = [a0 ; a1 .. an] and lst_2 = [b0 ; b1 .. bm] as the list 
      [(a0, b0); (a0, b1) .. (a1, b0); (a1, b1) .. (an, bm)]
      
      For this question, write a function to produce the cartesion product.
      
      [7 Points]  
*)

let rec cartesion_product lst_1 lst_2 = 
  list_rev @: 
    list_flatten @:
      list_map (fun e1 -> list_map (fun e2 -> (e1, e2)) lst_2) lst_1
;; (* ANSWER *)

(*
# cartesion_product [1; 2 ;3] [4 ; 5] ;;
- : (int * int) list = [(1, 4); (1, 5); (2, 4); (2, 5); (3, 4); (3, 5)]
# cartesion_product ["a"; "b"] [1 ; 2 ; 3];;
- : (string * int) list = [("a", 1); ("a", 2); ("a", 3); ("b", 1); ("b", 2); ("b", 3)]
# cartesion_product  ["a"; "b"] [] ;;
- : (string * 'a) list = []
*)

(*
  1e. Given a number n >= 1, write a function to compute the first n rows of Pascal's triangle
      (https://en.wikipedia.org/wiki/Pascal%27s_triangle)
      
      [8 Points]
*) 


let pascal n = 
  let rec next_row row =
    match row with 
    | fst :: ( snd :: tail as t) -> (fst+snd) :: next_row t
    | _ -> []
  in
  let rec pascal_helper i row =
    if i = n then []
    else row :: pascal_helper (i+1) (next_row (0 :: row @ [0]) )
  in pascal_helper 0 [1]
;; (* ANSWER *)
    
(*
# pascal 3 ;;
- : int list list = [[1]; [1; 1]; [1; 2; 1]]
# pascal 6 ;;
- : int list list =
[[1]; [1; 1]; [1; 2; 1]; [1; 3; 3; 1]; [1; 4; 6; 4; 1]; [1; 5; 10; 10; 5; 1]]
*)

(* -------------------------------------------------------------------------------------------------- *)
(* Section 2 : Sorting                                                                                *)
(* -------------------------------------------------------------------------------------------------- *)

(*
  2a. Given two sorted lists, merge them in to a single sorted list.
  
      It is possible to use the <= operator for comparison since it is polymorphic. However we can
      generalize this a bit further by allowing the caller to supply a comparison function. The 
      comparison function is expected to have the type (a -> a -> bool) and return true if the first
      parameter is less than or equal to the second. See test cases below.
  
      [7 Points]
*)

let rec merge_lists comparison_fn lst_1 lst_2 = 
  match lst_1, lst_2 with
  | [], _ -> lst_2
  | _, [] -> lst_1
  | h1 :: t1, h2 :: t2 -> 
      if comparison_fn h1 h2 then 
        h1 :: merge_lists comparison_fn t1 lst_2
      else
        h2 :: merge_lists comparison_fn lst_1 t2
;; 

(*
# merge_lists (<) [1;4;5;6;9] [2;3;6;7;8;10;11]
- : int list = [1; 2; 3; 4; 5; 6; 6; 7; 8; 9; 10; 11]
# merge_lists (fun p1 -> fun p2 -> fst p1 < fst p2) [('a',8);('c',1);('f',3)] [('b',3);('e', 7)]
- : (char * int) list = [('a', 8); ('b', 3); ('c', 1); ('e', 7); ('f', 3)]
*)

(*
  2b. Now write a function to sort a given list using the mergesort algorithm. You take a comparison function
      (similiar to the one used in question 3a) and a list of data as input.
      
      [8 Points]
*)

let rec mergesort comparison_fn lst = 
  match lst with
  | [] -> []
  | [x] -> [x]
  | xs -> 
      let fst_half, snd_half = list_split xs @: (list_length xs)/2  in
      merge_lists comparison_fn 
                  (mergesort comparison_fn fst_half) 
                  (mergesort comparison_fn snd_half)
;; 


(*
# mergesort (<) [5;7;2;3;1;5;9;4] ;;
- : int list = [1; 2; 3; 4; 5; 5; 7; 9]
# mergesort (fun p1 -> fun p2 -> fst p1 < fst p2) [('a',8);('c',1);('f',3);('b',3);('e', 7)] ;;
- : (char * int) list = [('a', 8); ('b', 3); ('c', 1); ('e', 7); ('f', 3)]
*)


(* -------------------------------------------------------------------------------------------------- *)
(* Section 3 : Encoding and Decoding                                                                  *)
(* -------------------------------------------------------------------------------------------------- *)

(*
  3a. Given a list of values, produce a run-length encoding of it. The result should be a list of pairs 
      where each one has the form (count, value)
      
      [8 Points]
*) 

let run_length_encode lst =  
  let rec aux count acc = function
    | [] -> []
    | [x] -> (count+1, x) :: acc
    | fst :: (snd :: _ as t) -> if fst = snd then aux (count+1) acc t
                                else aux 0 ( (1+count, fst) :: acc) t
  in list_rev (aux 0 [] lst)
;; (* ANSWER *)

(*
# run_length_encode ['F';'F';'F';'O';'O';'B';'A';'A';'R';'R';'R']
- : (int * char) list = [(3, 'F'); (2, 'O'); (1, 'B'); (2, 'A'); (3, 'R')]
# run_length_encode [1;2;3;1;2;1;1;2;2;2;0]
- : (int * int) list = [(1, 1); (1, 2); (1, 3); (1, 1); (1, 2); (2, 1); (3, 2); (1, 0)]
*)

(*
  3b. Given a run-length encoded list as (count, value) pairs, like the one produced from run_length_encode, 
      return a decoded list 
      
      [7 Points]
*) 

let run_length_decode lst = 
  let rec many acc n x = 
    if  n = 0 then acc else many (x :: acc) (n-1) x 
  in
  let rec aux acc = function
    | [] -> acc
    | (count, value) :: t -> (many [] count value) :: (aux acc t)
  in
  list_flatten @: aux [] lst
;; (* ANSWER *)

(*
# run_length_decode [(3, 'F'); (2, 'O'); (1, 'B'); (2, 'A'); (3, 'R')]
- : char list = ['F'; 'F'; 'F'; 'O'; 'O'; 'B'; 'A'; 'A'; 'R'; 'R'; 'R']
# run_length_decode [(1, 1); (1, 2); (1, 3); (1, 1); (1, 2); (2, 1); (3, 2); (1, 0)]
- : int list = [1; 2; 3; 1; 2; 1; 1; 2; 2; 2; 0]
*)

(* -------------------------------------------------------------------------------------------------- *)
(* Section 4 : Composing Functions                                                                       *)
(* -------------------------------------------------------------------------------------------------- *)


(*
  4. Functions can be composed with themselves, a process that can be repeated indefinitely.
     f o f is usually denoted as f^2 (^2, indicating superscript 2), f o f o f as f^3 and
     so forth.

     Write a function that takes a base function f and an integer n as parameters and produces
     the list [ f ; f^2 .. f^n ]. You can assume that n is at least 1

     [10 Points]
*)

let rec generate_compositions f n = 
  if n = 1 then [f]
  else f :: list_rev 
              ( list_map
                ( fun g -> (fun x -> f (g x)) )  @:
                generate_compositions f (n-1) 
              )
;; (* ANSWER *)

(*
# let rec map_all fn_list start = match fn_list with [] -> [] | f::t -> (f start)::(map_all t start) ;;
# let fn_list = generate_compositions (fun x -> x + 1) 5 ;;
val fn_list : (int -> int) list = [<fun>; <fun>; <fun>; <fun>; <fun>]
# map_all fn_list 0 ;;
- : int list = [1; 2; 3; 4; 5]
# let fn_list = generate_compositions (fun x -> x / 3) 5 ;;
val fn_list : (int -> int) list = [<fun>; <fun>; <fun>; <fun>; <fun>]
# map_all fn_list 100 ;;
- : int list = [33; 11; 3; 1; 0]
*)


(* -------------------------------------------------------------------------------------------------- *)
(* Section 5 : Packing Material                                                                       *)
(* -------------------------------------------------------------------------------------------------- *)

(*
  In this section we will build the core of a simple (simplistic?) package management system.
  
  Generally package management systems require you to declaratively specify for each package a package 
  identifier as well any dependencies on other packages. (Most sane package management systems also require 
  version information; but we will ignore those for simplicity)
  
  We represent this information ("package configuration") as a list of key-value pairs - the first element 
  (the "key") of each pair is the package identifier (represented as a string) and the second element is the 
  list of package (identifiers) that it depends on.
  
  For example, the list 
    [("Package-A", ["Package-B"; "Package-C"]);
     ("Package-B", ["Package-C"]);
     ("Package-C", ["Package-D"]);
     ("Package-D", [])
    ]
  represents a system where Package-A depends on both Package-B and Package-C, Package-B also depends on 
  Package-C and Package-C only depends on Package-D. The latter has no dependencies.
*) 

type package_info = string * string list ;;

(*  
  5a. In the example above, Package-A has an indirect dependency on Package-D (since Package-A depends on Package-C
      which in turn depends on Package-D.
      
      Given two package names (package_a and package_b), determine whether package_a has any dependency on 
      package_b
      
      [9 Points]
*) 

(* package_info list -> string -> string -> bool *)
let is_dependent_on package_config package_a package_b = 
  let rec dfs current visited = 
    try 
      let (_, dependencies) = list_find (fun (x, _) -> x = current) package_config in
      let rec loop = function
        | [] -> false
        | h::l -> h = package_b || 
                  ( not @: list_contain h visited) && (dfs h @: h::visited) ||
                  loop l
      in
      loop dependencies
    with Not_found -> false
  in
  dfs package_a []

;;

(* Here we define a few package configurations which are useful for testing all the following functions *)

(*
# let config_1 = 
    [("Package-A", ["Package-D"]);
     ("Package-B", ["Package-C"]);
     ("Package-C", ["Package-D"; "Package-E"]);
     ("Package-D", []);
     ("Package-E", [])
    ] ;;

# let config_2 = 
  [("Package-A", ["Package-D"; "Package-E"]);
   ("Package-B", ["Package-D"]);
   ("Package-C", ["Package-E"; "Package-J"]);
   ("Package-D", ["Package-F"; "Package-H"; "Package-J"]);
   ("Package-E", ["Package-H"]);
   ("Package-F", []);
   ("Package-H", []);
   ("Package-J", [])] ;;
*)

(*
# is_dependent_on config_1 "Package-A" "Package-E" ;;
- : bool = false
# is_dependent_on config_2 "Package-C" "Package-F" ;;
- : bool = false
# is_dependent_on config_2 "Package-A" "Package-H" ;;
- : bool = true
*) 

(*
  5b. As we saw in 4a, packages can have deep dependencies. In fact the dependencies can be circular (which can 
      be an issue when trying to install them).
      
      E.g. 
      [("Package-A", ["Package-B"; "Package-C"]);
       ("Package-B", ["Package-C"]);
       ("Package-C", ["Package-D"]);
       ("Package-D", ["Package-B"])
      ]
      indicates a package configuration with a circular dependency. (Package-B indirectly depends on Package-D 
      and Package-D depends on Package-B.
      
      For this question, given a package configuration, determine whether it has any circular dependencies.
      
      [9 Points]
*) 

(* package_info list -> bool *)
let has_circular_dependency package_config =
  let rec aux = function
    | [] -> false
    | (name, _)::l -> is_dependent_on package_config name name || aux l
  in
  aux package_config
;; (* ANSWER *)

(*
# has_circular_dependency config_1 ;;
- : bool = false
# has_circular_dependency config_2 ;;
- : bool = false
# let config_circ_1 = 
  [("Package-A", ["Package-D"]);
   ("Package-B", ["Package-C"]);
   ("Package-C", ["Package-D"; "Package-E"]);
   ("Package-D", ["Package-B"]);
   ("Package-E", ["Package-A"])] ;;
# has_circular_dependency config_circ_1 ;;
- : bool = true
# let config_circ_2 = 
  [("Package-A", ["Package-D"; "Package-E"]);
   ("Package-B", ["Package-D"]);
   ("Package-C", ["Package-E"; "Package-J"]);
   ("Package-D", ["Package-F"; "Package-J"]);
   ("Package-E", ["Package-H"]);
   ("Package-F", []);
   ("Package-H", ["Package-C"]);
   ("Package-J", [])] ;;
# has_circular_dependency config_circ_2;;
- : bool = true
*)

(*
  5c. A package can only be installed if all the packages it depends on are installed. Assuming that the 
      package configuration has no circular dependencies, it should be possible to install any package as
      long as you install the dependencies in the right order.
      
      For this question you are given the identifier for a package to install as well as a list of already
      installed packages. Your task is to return the list of packages to be installed in the order in which
      they should be installed.
      
      Note: Depending on the package configuration, there may be multiple orders in which the packages can be
      installed. The order generated by your code does not have to precisely match the examples below. It just
      needs to be "correct".  
      
      [12 Points]  
*) 

(* remove duplciate in sorted list*)
let rec compress = function
      | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
          | smaller -> smaller;;

(* package_info list -> string -> string list -> string list *)
let get_packages_to_install package_config package_id installed_packages = 
  let rec dfs current = 
    try 
      let (_, dependencies) = list_find (fun (x, _) -> x = current) package_config in
      let rec loop = function
        | [] -> []
        | h::l -> (if list_contain h installed_packages then [] 
                  else [h]) @
                  (dfs h) @ (loop l)
      in
      loop dependencies
    with Not_found -> []
  in
  compress @: package_id :: ( mergesort (>) (dfs package_id) )
;;

(*
# get_packages_to_install config_1 "Package-B" [] ;;
- : string list = ["Package-D"; "Package-E"; "Package-C"; "Package-B"]
# get_packages_to_install config_2 "Package-A" [] ;;
- : string list = ["Package-F"; "Package-H"; "Package-J"; "Package-D"; "Package-E"; "Package-A"]
# get_packages_to_install config_2 "Package-A" ["Package-H"; "Package-E"] ;;
- : string list = ["Package-F"; "Package-J"; "Package-D"; "Package-A"]
# get_packages_to_install config_2 "Package-C" ["Package-J"] ;;
- : string list = ["Package-H"; "Package-E"; "Package-C"]
*) 
