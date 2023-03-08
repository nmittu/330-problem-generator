type ocaml_type =
  | Int
  | Float
  | String
  | Bool
  | Tuple of ocaml_type * ocaml_type
  | List of ocaml_type
  | Func of (ocaml_type * ocaml_type)
  | Polymorphic of string

type mode = [`Non_polymorphic | `Polymorphic of int]
type typ = [`Func of int | `List of int | `Norm]

val generate : mode:mode -> typ:typ -> int -> ocaml_type
val to_string : ocaml_type -> string
