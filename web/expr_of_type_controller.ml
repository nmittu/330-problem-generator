open! Core
open Compiler
open Controller_intf

type t =
  { typ : string
  ; mode_config : [ `Non_polymorphic | `Polymorphic ]
  ; poly_count : int
  ; typ_config : [ `Norm | `Func ]
  ; func_count : int
  ; params : int
  }
[@@deriving sexp, compare]

type settings_action =
  | Update_mode of Type_generator.mode
  | Update_typ of Type_generator.typ
  | Update_params of int
[@@deriving sexp]

let init () =
  { typ =
      Type_generator.generate ~mode:(`Polymorphic 3) ~typ:(`Func 1) 4
      |> Type_generator.to_string
  ; mode_config = `Polymorphic
  ; poly_count = 3
  ; typ_config = `Func
  ; func_count = 1
  ; params = 4
  }
;;

(** Validates the settings for this controller and updates any values
    if necesary. *)
let validate_conf t =
  let poly_count = min t.poly_count t.params |> max 1 in
  let func_count = min t.func_count (t.params - 1) |> max 1 in
  let params = max t.params 2 in
  { t with poly_count; func_count; params }
;;

let mode t =
  if Poly.(`Polymorphic = t.mode_config)
  then `Polymorphic t.poly_count
  else `Non_polymorphic
;;

let typ t = if Poly.(`Func = t.typ_config) then `Func t.func_count else `Norm

let next t =
  let t = validate_conf t in
  { t with
    typ =
      Type_generator.generate ~mode:(mode t) ~typ:(typ t) t.params
      |> Type_generator.to_string
  }
;;

let problem t = t.typ
let should_submit _ = String.is_suffix ~suffix:";;\n"
let type_regex = Str.regexp {|[a-zA-Z- ]* : \(.*\) = .*|}
let remove_excess_whitespace = Str.regexp "[ \n]+"

let submit t input =
  let input = String.strip input in
  match execute input with
  | Ok compile_result ->
    let compile_result = Str.global_replace remove_excess_whitespace " " compile_result in
    if Str.string_match type_regex compile_result 0
    then (
      let type_ = Str.matched_group 1 compile_result in
      if String.(type_ = t.typ)
      then Ok type_
      else Error (Printf.sprintf "Incorrect type: %s" type_))
    else Error compile_result
  | Error error -> Error error
;;

let on_tab _ target = Indent.textarea target |> ignore

let settings t =
  [ Group
      { name = "Parameter Type"
      ; settings =
          [ Bool
              { name = "Non Polymorphic"
              ; enabled = Some Poly.(t.mode_config = `Non_polymorphic)
              ; value = ()
              ; update = (fun () -> Update_mode `Non_polymorphic)
              ; extra = ()
              }
          ; With_int
              { name = "Polymorphic"
              ; enabled = Some Poly.(t.mode_config = `Polymorphic)
              ; value = t.poly_count
              ; update = (fun c -> Update_mode (`Polymorphic c))
              ; extra = 1, t.params
              }
          ]
      }
  ; Group
      { name = "Higher order functions config"
      ; settings =
          [ Bool
              { name = "Basic"
              ; enabled = Some Poly.(t.typ_config = `Norm)
              ; value = ()
              ; update = (fun () -> Update_typ `Norm)
              ; extra = ()
              }
          ; With_int
              { name = "HOF (number of functions)"
              ; enabled = Some Poly.(t.typ_config = `Func)
              ; value = t.func_count
              ; update = (fun c -> Update_typ (`Func c))
              ; extra = 1, t.params - 1
              }
          ]
      }
  ; Int
      { name = "Number of params"
      ; enabled = None
      ; value = t.params
      ; update = (fun c -> Update_params c)
      ; extra = 2, 26
      }
  ]
;;

let update_mode t mode =
  let t =
    match mode with
    | `Polymorphic n -> { t with mode_config = `Polymorphic; poly_count = n }
    | `Non_polymorphic -> { t with mode_config = `Non_polymorphic }
  in
  next t
;;

let update_typ t typ =
  let t =
    match typ with
    | `Func n -> { t with typ_config = `Func; func_count = n }
    | `Norm -> { t with typ_config = `Norm }
  in
  next t
;;

let update_params t params = next { t with params }

let update_settings t action =
  match action with
  | Update_mode mode -> update_mode t mode
  | Update_typ typ_ -> update_typ t typ_
  | Update_params p -> update_params t p
;;
