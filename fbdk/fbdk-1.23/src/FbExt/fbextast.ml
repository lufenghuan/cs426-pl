type ident = Ident of string

type expr = 
 Var of ident | Function of ident * expr | Appl of expr * expr |
 LetRec of ident * ident * expr * expr |
 Plus of expr * expr | Minus of expr * expr | Equal of expr * expr | 
 And of expr * expr| Or of expr * expr | Not of expr |  
 If of expr * expr * expr | Int of int | Bool of bool

type fbtype = TInt | TBool | TArrow of fbtype * fbtype | TVar of string;;

