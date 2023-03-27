open! Core
include Generator.Generator

type _ value
type _ typ

val int : int typ
val float : float typ
val bool : bool typ
val string : string typ
val list : 'a typ -> 'a list typ

type _ expr =
  | IntVar : int expr
  | FloatVar : float expr
  | StringVar : string expr
  | BoolVar : bool expr
  | Const : 'a value -> 'a expr
  | List : 'a expr list -> 'a list expr
  | Cons : 'a expr * 'a list expr -> 'a list expr
  | Int_of_string : string expr -> int expr
  | Int_of_float : float expr -> int expr
  | Float_of_int : int expr -> float expr
  | Float_of_string : string expr -> float expr
  | String_of_float : float expr -> string expr
  | String_of_int : int expr -> string expr
  | Concat : string expr * string expr -> string expr
  | Plus : int expr * int expr -> int expr
  | Minus : int expr * int expr -> int expr
  | Mult : int expr * int expr -> int expr
  | Plus_dot : float expr * float expr -> float expr
  | Minus_dot : float expr * float expr -> float expr
  | Mult_dot : float expr * float expr -> float expr
  | Eq : 'a expr * 'a expr -> bool expr
  | Gt : 'a expr * 'a expr -> bool expr
  | Lt : 'a expr * 'a expr -> bool expr
  | Or : bool expr * bool expr -> bool expr
  | And : bool expr * bool expr -> bool expr
  | Not : bool expr -> bool expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr

val generate : ?forced:bool -> 'a typ -> int -> 'a expr
val to_string : ?max_cat:int -> 'a expr -> string
