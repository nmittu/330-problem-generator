open! Core

(** Parses the given ocaml type.
    @returns a [result] that contains either the parsed type, or a string error message.*)
val parse : string -> (Type_intf.ocaml_type, string) result
