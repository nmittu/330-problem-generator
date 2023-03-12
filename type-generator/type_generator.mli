type ocaml_type =
  | Int
  | Float
  | String
  | Bool
  | Tuple of ocaml_type * ocaml_type
  | List of ocaml_type
  | Func of (ocaml_type * ocaml_type)
  | Polymorphic of string

type mode =
  [ `Non_polymorphic
  | `Polymorphic of int
  ]
[@@deriving sexp, compare]

type typ =
  [ `Func of int
  | `Norm
  ]
[@@deriving sexp, compare]

val generate : mode:mode -> typ:typ -> int -> ocaml_type
val to_string : ocaml_type -> string
