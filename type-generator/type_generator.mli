module Parser = Parser

type ocaml_type = Type_intf.ocaml_type

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

val normalize : ocaml_type -> ocaml_type
val generate : mode:mode -> typ:typ -> int -> ocaml_type
val to_string : ocaml_type -> string
