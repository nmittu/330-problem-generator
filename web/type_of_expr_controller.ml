open! Core
open Compiler
open Controller_intf
open Expr_generator

type t =
  { expr : string
  ; depth : int
  ; mode : [ `Polymorphic | `Non_polymorphic ]
  }
[@@deriving sexp, compare]

type settings_action =
  | Update_depth of int
  | Update_mode of [ `Polymorphic | `Non_polymorphic ]
[@@deriving sexp]

let get_module m : (module Generator) =
  match m with
  | `Polymorphic -> (module Polymorphic)
  | `Non_polymorphic -> (module Non_polymorphic)
;;

let generate_expr m depth =
  let open (val get_module m) in
  gen_string depth
;;

let init () =
  { expr = generate_expr `Non_polymorphic 2; depth = 2; mode = `Non_polymorphic }
;;

let next t = { t with expr = generate_expr t.mode t.depth }
let problem t = t.expr
let should_submit _ = String.is_suffix ~suffix:"\n"
let type_regex = Str.regexp {|[a-zA-Z- ]* : \(.*\) = .*|}
let remove_excess_whitespace = Str.regexp "[ \n]+"

let submit t input =
  let open Result.Let_syntax in
  let expected = execute (t.expr ^ ";;") in
  let%bind expected =
    let%bind expected = expected in
    let expected = Str.global_replace remove_excess_whitespace " " expected in
    if Str.string_match type_regex expected 0
    then (
      let typ = Str.matched_group 1 expected |> String.strip in
      return (Type_generator.Parser.parse typ))
    else Error ("Invalid problem, please let course staff know.\nerror: " ^ expected)
  in
  let typ = Type_generator.Parser.parse input >>| Type_generator.normalize in
  match expected, typ with
  | Error e, _ ->
    Error
      ("There is an issue with this problem, please let course staff know.\nerror: " ^ e)
  | _, Error e -> Error ("Invalid type: " ^ e)
  | Ok expected, Ok typ ->
    if Poly.(expected = typ) then Ok "Correct!" else Error "Incorrect type"
;;

let on_tab _ _ = ()

let settings t =
  [ Group
      { name = "Parameter Type"
      ; settings =
          [ Bool
              { name = "Non Polymorphic"
              ; enabled = Some Poly.(t.mode = `Non_polymorphic)
              ; value = ()
              ; update = (fun () -> Update_mode `Non_polymorphic)
              ; extra = ()
              }
          ; Bool
              { name = "Polymorphic"
              ; enabled = Some Poly.(t.mode = `Polymorphic)
              ; value = ()
              ; update = (fun () -> Update_mode `Polymorphic)
              ; extra = ()
              }
          ]
      }
  ; Int
      { name = "Expression depth "
      ; enabled = None
      ; value = t.depth
      ; update = (fun d -> Update_depth d)
      ; extra = 2, 5
      }
  ]
;;

let update_settings t action =
  match action with
  | Update_depth depth -> next { t with depth }
  | Update_mode mode -> next { t with mode }
;;
