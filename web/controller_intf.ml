open! Core

type ('v, 'e, 'a) setting =
  { name : string
  ; enabled : bool option
  ; value : 'v
  ; update : 'v -> 'a
  ; extra : 'e
  }

type 'a radio_settings =
  | Bool of (unit, unit, 'a) setting
  | With_int of (int, int * int, 'a) setting

type 'a settings_group =
  { name : string
  ; settings : 'a radio_settings list
  }

type 'a settings =
  | Group of 'a settings_group
  | Int of (int, int * int, 'a) setting

module type S = sig
  type t [@@deriving sexp, compare]
  type settings_action [@@deriving sexp]

  val init : unit -> t
  val next : t -> t
  val problem : t -> string
  val should_submit : t -> string -> bool
  val submit : t -> string -> (string, string) result
  val on_tab : t -> Js_of_ocaml.Dom_html.textAreaElement Js_of_ocaml.Js.t -> unit
  val settings : t -> settings_action settings list
  val update_settings : t -> settings_action -> t
end
