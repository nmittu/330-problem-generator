open! Core

(** This {{:https://blog.janestreet.com/howto-static-access-control-using-phantom-types/}phantom type}
    represents the type of functions in our expressions. We use a phantom type here because, we do
    not know (or need to know) the type of the parameters or return value of functions in our expressions.
    We need to use a phantom type however to make sure that certain expressions such as lists or tuples
    are used as functions. *)
type func

(** This {{:https://dev.realworldocaml.org/gadts.html}GADT} represents Ocaml expressions *)
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

(** This {{:https://blog.janestreet.com/howto-static-access-control-using-phantom-types/}phantom type}
    represents expressions that have a completely polymorphic type. For example [x] or [if a then b else c]
    would have a completely polymorphic type, however [[x]] or [(a, b)] would not.*)
type polyVal

(** This {{:https://dev.realworldocaml.org/gadts.html}GADT} represents the types of expressions *)
type _ typ =
  | AT : polyVal typ
  | ListT : 'a typ -> 'a list typ
  | TupleT : 'a typ * 'b typ -> ('a * 'b) typ
  | FuncT : func typ
  | BoolT : bool typ

include Generator.Generator

(** Generates a random expression.
    @param typ the {!typ} of the expression that will be generated
    @param depth the depth of the expression that will be generated.*)
val generate : 'a typ -> int -> 'a expr

(** Converts an expression to a string. *)
val to_string : 'a expr -> string
