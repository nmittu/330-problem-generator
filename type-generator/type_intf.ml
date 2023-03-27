type ocaml_type =
  | Int
  | Float
  | String
  | Bool
  | Tuple of ocaml_type * ocaml_type
  | List of ocaml_type
  | Func of (ocaml_type * ocaml_type)
  | Polymorphic of string
