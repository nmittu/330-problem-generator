open! Core

(** This module allows you to execute OCaml code in a
    {!Js_of_ocaml_toplevel} *)

(** Executes the [string] in the toplevel. Returns [Ok s]
    with the output of from the toplevel if no errors occured
    Returns [Error s] if the toplevel printed anything to [stderr].*)
val execute : string -> (string, string) result
