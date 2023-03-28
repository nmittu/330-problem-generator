open! Core

module type Generator = sig
  (** This module is implemented by type generators such as
      {!Polymorphic} and {!Non_polymorphic}. *)

  (** Generates the string representation of an expression with the given depth. *)
  val gen_string : int -> string
end
