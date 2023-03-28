module Parser = Parser
module Type_intf = Type_intf

type ocaml_type = Type_intf.ocaml_type

(** The {!mode} controls whether the generated type will have polymorphic
    or concrete types.*)
type mode =
  [ `Non_polymorphic
  | `Polymorphic of int
  ]
[@@deriving sexp, compare]

(** The {!typ} controls whether the type should contain higher order functions
    as arguments or not. *)
type typ =
  [ `Func of int
  | `Norm
  ]
[@@deriving sexp, compare]

(** This function normalizes types so that all polymorphic type parameters
    have the same name that the Ocaml compiler would give tham. *)
val normalize : ocaml_type -> ocaml_type

(** Generates an ocaml type with the given {!mode} and {!typ} and number of parameters. *)
val generate : mode:mode -> typ:typ -> int -> ocaml_type

(** Converts an ocaml type to a string *)
val to_string : ocaml_type -> string
