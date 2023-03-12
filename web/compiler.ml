open! Core
open Js_of_ocaml_toplevel
open Js_of_ocaml

let () =
  Clflags.debug := true;
  JsooTop.initialize ()

let execute code =
  let buffer = Buffer.create 500 in
  let formatter = Format.formatter_of_buffer buffer in
  let error = ref "" in
  let flusher s =
    error := !error ^ s
  in
  Sys_js.set_channel_flusher stderr flusher;
  JsooTop.execute true formatter code;
  if String.is_empty !error then
    Ok (Buffer.contents buffer)
  else
    Error !error
