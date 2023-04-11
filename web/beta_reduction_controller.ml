open! Core
open Controller_intf
open Lambda_calc

type t =
  { expr : expr
  ; depth : int
  }
[@@deriving sexp, compare]

type settings_action = Update_depth of int [@@deriving sexp]

let init () = { expr = Generator.generate 3; depth = 3 }
let next t = { t with expr = Generator.generate t.depth }
let problem t = to_string t.expr
let should_submit _ = String.is_suffix ~suffix:"\n"

let submit t input =
  let open Result.Let_syntax in
  let expected = t.expr |> Generator.reduce |> Generator.normalize in
  let%bind submitted = Parser.parse input >>| Generator.normalize in
  if Poly.(expected = submitted) then return "Correct!" else Error "Incorrect"
;;

let on_tab _ _ = ()

let settings t =
  [ Int
      { name = "Expression depth "
      ; enabled = None
      ; value = t.depth
      ; update = (fun d -> Update_depth d)
      ; extra = 2, 50
      }
  ]
;;

let update_settings t action =
  match action with
  | Update_depth depth -> next { t with depth }
;;
