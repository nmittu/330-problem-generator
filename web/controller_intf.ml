open! Core

(** The controller interface manages the specific logig for
    each problem type. Implementing a new problem type will require
    creating a new module that satisfies the {!S} signature*)

(** This represents a single option in the settings for this problem type.
    - ['v] is the type of the value of this option
    - ['e] is the type of the extra values associated with the option
    - ['a] is the type of the action to update this option *)
type ('v, 'e, 'a) setting =
  { name : string
  ; enabled : bool option
      (** Whether the radio box should be selected. If this is [None],
          no radio box will be rendered. *)
  ; value : 'v
  ; update : 'v -> 'a
      (** Function that returns the action that should be taken when a user
          selects this option. *)
  ; extra : 'e
      (** Extra data associated with this option. For {!Int} options, this
          is the min and max values. *)
  }

(** A setting in a radio group *)
type 'a radio_settings =
  | Bool of (unit, unit, 'a) setting
  | With_int of (int, int * int, 'a) setting

(** A group of radio options *)
type 'a settings_group =
  { name : string
  ; settings : 'a radio_settings list
  }

(** Settings can either be a group of radio options or a single int option. *)
type 'a settings =
  | Group of 'a settings_group
  | Int of (int, int * int, 'a) setting

module type S = sig
  (** The interface that must be implemented to create a problem type. *)

  (** Type of the data associated with this problem controller *)
  type t [@@deriving sexp, compare]

  (** The actions that can be taken by updating the settins of this controller. *)
  type settings_action [@@deriving sexp]

  (** Get an controler that will be used to show the first problem. *)
  val init : unit -> t

  (** Generate the next problem. *)
  val next : t -> t

  (** Get the text of the current problem. *)
  val problem : t -> string

  (** Checks whether the problem should submit given the current text in the
      input. This function will often check for a newline character to see if
      the user has clicked the enter button. If {!should_submit} returns true,
      leading and trailing whitespace will be removed from the input. *)
  val should_submit : t -> string -> bool

  (** Submit the answer to the current problem. If the returned [result] is [Ok s]
      then the string [s] will be displayed in green text. If [Error s] is returned
      [s] will be displayed in red text. *)
  val submit : t -> string -> (string, string) result

  (** This function is called whenever the user clicks the tab button while the
      input box is focused. This allows you to perform actions and modifications to
      the contents of the text box. *)
  val on_tab : t -> Js_of_ocaml.Dom_html.textAreaElement Js_of_ocaml.Js.t -> unit

  (** The {!type-settings} associated with this controller. *)
  val settings : t -> settings_action settings list

  (** Updates a setting in the controller. *)
  val update_settings : t -> settings_action -> t
end
