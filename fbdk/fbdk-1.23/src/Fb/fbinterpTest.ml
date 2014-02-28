(**************************************************************************)
(* The OUnit library                                                      *)
(*                                                                        *)
(* Copyright (C) 2002-2008 Maas-Maarten Zeeman.                           *)
(* Copyright (C) 2010 OCamlCore SARL                                      *)
(* Copyright (C) 2013 Sylvain Le Gall                                     *)
(*                                                                        *)
(* The package OUnit is copyright by Maas-Maarten Zeeman, OCamlCore SARL  *)
(* and Sylvain Le Gall.                                                   *)
(*                                                                        *)
(* Permission is hereby granted, free of charge, to any person obtaining  *)
(* a copy of this document and the OUnit software ("the Software"), to    *)
(* deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute,           *)
(* sublicense, and/or sell copies of the Software, and to permit persons  *)
(* to whom the Software is furnished to do so, subject to the following   *)
(* conditions:                                                            *)
(*                                                                        *)
(* The above copyright notice and this permission notice shall be         *)
(* included in all copies or substantial portions of the Software.        *)
(*                                                                        *)
(* The Software is provided ``as is'', without warranty of any kind,      *)
(* express or implied, including but not limited to the warranties of     *)
(* merchantability, fitness for a particular purpose and noninfringement. *)
(* In no event shall Maas-Maarten Zeeman be liable for any claim, damages *)
(* or other liability, whether in an action of contract, tort or          *)
(* otherwise, arising from, out of or in connection with the Software or  *)
(* the use or other dealings in the software.                             *)
(*                                                                        *)
(* See LICENSE.txt for details.                                           *)
(**************************************************************************)

open OUnit
#use "fbdktoploop.ml";;

let e1 =  
  "Let Rec x1 x2 = 
     If x2 = 1 Then
        (Function x3 -> x3 (x2 - 1)) (Function x4 -> x4)
     Else
        x1 (x2 - 1)
   In x1 100"
;;

let e2 = "Function x -> x + 1"  ;;
let e3 = "Function x-> Function y -> x";;
let e4 = "Function x-> Function x -> x + x";;
let e5 = "If 1 = 2 Then True Else False";;
let e6 = "(Function x -> x 4 5) (Function x -> Function y -> x)";;

let closed = e1 :: e2 :: e3 :: e4 :: e5 :: e6 :: [];;

(* not closed expression*)
let f1 = "If x = 2 Then True Else False";;
let f2 = "x + 1";;
let f3 = "Function x-> Function y -> x + z";;
let f4 = 
  "Let Rec x1 x2 = 
     If x2 = 1 Then
        (Function x3 -> x3 (x2 - 1)) (Function x4 -> x4)
     Else
        x1 (x2 - 1)
   In x8 100"
;;

let non_closed = f1 :: f2 :: f3 :: f4 :: [];;


let test_check_closed _ = 
  List.iter (fun x -> assert_equal true (check_closed (parse x)) ) closed;
  List.iter (fun x -> assert_equal false (check_closed (parse x)) ) non_closed
;;



let suite = "Test fbinterp" >::: ["test_closed" >:: test_check_closed]
let _ =
  run_test_tt_main suite
