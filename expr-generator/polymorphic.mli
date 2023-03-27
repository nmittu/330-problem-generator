open! Core
include Generator.Generator

type func

type _ expr =
  | Var : 'a expr
  | List : 'a expr list -> 'a list expr
  | Cons : 'a expr * 'a list expr -> 'a list expr
  | Tuple : 'a expr * 'b expr -> ('a * 'b) expr
  | App : func expr * 'a expr list -> 'a expr
  | Eq : 'a expr * 'a expr -> bool expr
  | Gt : 'a expr * 'a expr -> bool expr
  | Lt : 'a expr * 'a expr -> bool expr
  | Or : bool expr * bool expr -> bool expr
  | And : bool expr * bool expr -> bool expr
  | Not : bool expr -> bool expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr

type polyVal

type _ typ =
  | AT : polyVal typ
  | ListT : 'a typ -> 'a list typ
  | TupleT : 'a typ * 'b typ -> ('a * 'b) typ
  | FuncT : func typ
  | BoolT : bool typ

val generate : 'a typ -> int -> 'a expr
val to_string : 'a expr -> string
