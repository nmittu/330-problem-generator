open! Core
include Generator.Generator

type expr =
  | Var
  | List of expr list
  | Cons of expr * expr
  | Tuple of expr list
  | App of expr * expr list
  | Eq of expr * expr
  | Gt of expr * expr
  | Lt of expr * expr
  | Or of expr * expr
  | And of expr * expr
  | Not of expr
  | If of expr * expr * expr

val generate : int -> expr
val to_string : expr -> string
