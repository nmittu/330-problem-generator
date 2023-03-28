open! Core

(** This {{:https://dev.realworldocaml.org/gadts.html}GADT} represents a value in the AST. *)
type _ value =
  | Int : int -> int value
  | Float : float -> float value
  | Bool : bool -> bool value
  | String : string -> string value

(** This {{:https://dev.realworldocaml.org/gadts.html}GADT} represents the type of an expression.
    This type should be instantiated using {!int}, {!float}, {!bool}, {!string}, {!list}*)
type _ typ

val int : int typ
val float : float typ
val bool : bool typ
val string : string typ
val list : 'a typ -> 'a list typ

(** This {{:https://dev.realworldocaml.org/gadts.html}GADT} represents OCaml expressions. *)
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

include Generator.Generator

(** Generates a random expression with the provided {!typ} and depth.

    @param forced determines whether the type of the expression needs to be
    forced to have the given type. For example, [int_of_string x] would be forced to have the type [int]
    while [x] would not. By default this is [true] and should only be set
    to [false] when the expression is going to be inserted into some larger
    expression that will force the sub-expression to have the correct type. *)
val generate : ?forced:bool -> 'a typ -> int -> 'a expr

(** Converts an {!expr} to a [string].

    @param max_cat represents the maximum number of variables in the expression
    that can have the same type. By default this is [2]. *)
val to_string : ?max_cat:int -> 'a expr -> string
