open! Core
open Js_of_ocaml

(** Indents the given textbox using [ocp-indent] and the currently
    selected text or location of cursor in the textarea. *)
val textarea : Dom_html.textAreaElement Js.t -> string
