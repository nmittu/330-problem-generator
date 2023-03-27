open! Core

module type Generator = sig
  val gen_string : int -> string
end
