open! Core
open Compiler
open Controller_intf
open Expr_generator

type t =
  { expr : string
  ; depth : int
  }
[@@deriving sexp, compare]

type settings_action = Update_depth of int [@@deriving sexp]

let generate_expr depth =
  let open NonPolymorphic in
  let g t = generate t depth |> to_string in
  match Random.int 5 with
  | 0 -> g int
  | 1 -> g float
  | 2 -> g string
  | 3 -> g bool
  | _ ->
    (match Random.int 4 with
     | 0 -> g (list int)
     | 1 -> g (list float)
     | 2 -> g (list string)
     | _ -> g (list bool))
;;

let init () = { expr = generate_expr 3; depth = 3 }
let next t = { t with expr = generate_expr t.depth }
let problem t = t.expr
let should_submit _ = String.is_suffix ~suffix:"\n"
let type_regex = Str.regexp {|[a-zA-Z- ]* : \(.*\) = .*|}

let submit t input =
  let expected = execute (t.expr ^ ";;") in
  let typ = execute (Printf.sprintf "type t = %s;;" input) in
  match expected, typ with
  | Error e, _ ->
    Error
      ("There is an issue with this problem, please let course staff know.\nerror: " ^ e)
  | _, Error e -> Error ("Invalid type: " ^ e)
  | Ok expected, Ok typ ->
    if Str.string_match type_regex expected 0
    then (
      let expected = Str.matched_group 1 expected |> String.strip in
      let typ = String.chop_prefix_if_exists typ ~prefix:"type t = " |> String.strip in
      if String.(expected = typ) then Ok "Correct!" else Error "Incorrect type")
    else Error ("Invalid problem, please let course staff know.\nerror: " ^ expected)
;;

let on_tab _ _ = ()

let settings t =
  [ Int
      { name = "Expression depth "
      ; enabled = None
      ; value = t.depth
      ; update = (fun d -> Update_depth d)
      ; extra = 2, 10
      }
  ]
;;

let update_settings t action =
  match action with
  | Update_depth depth -> next { t with depth }
;;
