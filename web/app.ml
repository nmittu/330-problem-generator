open! Core
open Incr_dom
open Controller_intf
module Js = Js_of_ocaml.Js

module Make (Controller : Controller_intf.S) = struct
  module Model = struct
    type t =
      { controller : Controller.t
      ; input : string
      ; result : string
      ; error : string
      }
    [@@deriving sexp, fields, compare]

    let init () = { controller = Controller.init (); input = ""; result = ""; error = "" }

    let next t =
      { controller = Controller.next t.controller; input = ""; result = ""; error = "" }
    ;;

    let update_settings t a =
      { t with controller = Controller.update_settings t.controller a }
    ;;

    let submit t =
      match Controller.submit t.controller t.input with
      | Ok result -> { t with result }
      | Error error -> { t with error }
    ;;

    let update_input t input =
      let model = { t with input; result = ""; error = "" } in
      if Controller.should_submit t.controller input
      then submit { model with input = String.strip input }
      else model
    ;;

    let cutoff t1 t2 = compare t1 t2 = 0
  end

  module Action = struct
    type t =
      | Update_input of string
      | Next
      | Update_settings of Controller.settings_action
    [@@deriving sexp]
  end

  module State = struct
    type t = unit
  end

  let apply_action model action _ ~schedule_action:_ =
    match (action : Action.t) with
    | Update_input s -> Model.update_input model s
    | Next -> Model.next model
    | Update_settings a -> Model.update_settings model a
  ;;

  let on_startup ~schedule_action:_ _ = Async_kernel.return ()

  let handle_resize id =
    let open Option.Let_syntax in
    let open Js_of_ocaml.Dom_html in
    let opt =
      let%map target = Js.Opt.to_option (CoerceTo.textarea (getElementById id)) in
      target##.style##.height := Js.string "0";
      target##.style##.height
      := Js.string (Int.to_string (target##.scrollHeight + 5) ^ "px")
    in
    Option.value ~default:() opt
  ;;

  let gensym =
    let count = ref (-1) in
    let gensym ?(s = "gensym") () =
      incr count;
      s ^ Int.to_string !count
    in
    gensym
  ;;

  let build_bool_setting s inject =
    let open Vdom in
    let id = gensym ~s:"radio" () in
    let radio =
      match s.enabled with
      | None -> []
      | Some enabled ->
        [ Node.input
            ~attr:
              (Attr.many_without_merge
                 [ Attr.type_ "radio"
                 ; Attr.id id
                 ; Attr.bool_property "checked" enabled
                 ; Attr.on_click (fun _ -> inject (Action.Update_settings (s.update ())))
                 ])
            []
        ]
    in
    radio
    @ [ Node.label ~attr:(Attr.many_without_merge [ Attr.for_ id ]) [ Node.text s.name ]
      ; Node.br ()
      ]
  ;;

  let build_int_setting s inject =
    let open Vdom in
    let r_id = gensym ~s:"radio" () in
    let i_id = gensym ~s:"input" () in
    let get_int_val_by_id id =
      let open Option.Let_syntax in
      let open Js_of_ocaml.Dom_html in
      let%map target = Js.Opt.to_option (CoerceTo.input (getElementById id)) in
      target##.value |> Js.to_string |> Int.of_string
    in
    let on_input _ =
      match get_int_val_by_id i_id with
      | None -> Ui_effect.return ()
      | Some i -> inject (Action.Update_settings (s.update i))
    in
    let radio =
      match s.enabled with
      | None -> []
      | Some enabled ->
        [ Node.input
            ~attr:
              (Attr.many_without_merge
                 [ Attr.type_ "radio"
                 ; Attr.id r_id
                 ; Attr.bool_property "checked" enabled
                 ; Attr.on_click on_input
                 ])
            []
        ]
    in
    let min, max = s.extra in
    radio
    @ [ Node.label
          ~attr:(Attr.many_without_merge [ Attr.for_ r_id ])
          [ Node.text s.name
          ; Node.input
              ~attr:
                (Attr.many_without_merge
                   [ Attr.type_ "number"
                   ; Attr.id i_id
                   ; Attr.min (Int.to_float min)
                   ; Attr.max (Int.to_float max)
                   ; Attr.string_property "value" (Int.to_string s.value)
                   ; Attr.on_input (fun e _ -> on_input e)
                   ; Attr.on_keydown (fun _ -> Effect.Prevent_default)
                   ])
              []
          ]
      ]
  ;;

  let build_group g inject =
    let open Vdom in
    let div_css =
      let ( @> ) = Css_gen.( @> ) in
      Css_gen.display `Inline_block @> Css_gen.padding_right (`Em 2)
    in
    Node.div
      ~attr:(Attr.many_without_merge [ Attr.style div_css ])
      (Node.text g.name
       :: Node.br ()
       :: List.concat_map g.settings ~f:(function
            | Bool s -> build_bool_setting s inject
            | With_int s -> build_int_setting s inject))
  ;;

  let build_settings s inject =
    let open Vdom in
    Node.div
    @@ List.concat_map s ~f:(function
         | Group g -> [ build_group g inject ]
         | Int s -> build_int_setting s inject)
  ;;

  let view (m : Model.t Incr.t) ~inject =
    let open Incr.Let_syntax in
    let open Vdom in
    let next_button =
      Node.button
        ~attr:(Attr.many_without_merge [ Attr.on_click (fun _ -> inject Action.Next) ])
        [ Node.text "Next Problem" ]
    in
    let%map description_node =
      let%map desc = m >>| Model.controller >>| Controller.description in
      Node.pre [ Node.text desc ]
    and problem_node =
      let%map problem = m >>| Model.controller >>| Controller.problem in
      Node.pre [ Node.text problem ]
    and code_input =
      let%map code = m >>| Model.input
      and controller = m >>| Model.controller in
      let css =
        let ( @> ) = Css_gen.( @> ) in
        Css_gen.width (`Percent (Percent.of_percentage 100.)) @> Css_gen.resize `None
      in
      let module FocusHook = Attr.Single_focus_hook () in
      Node.textarea
        ~attr:
          (Attr.many_without_merge
             [ Attr.id "input"
             ; Attr.type_ "text"
             ; Attr.string_property "value" code
             ; Attr.string_property "id" "code_input"
             ; Attr.string_property "rows" "1"
             ; FocusHook.attr
                 `Read_the_docs__this_hook_is_unpredictable
                 ~after:Ui_effect.Ignore
             ; Attr.style css
             ; Attr.on_keydown (fun e ->
                 if e##.keyCode = 09
                 then (
                   let target =
                     Js.Opt.bind e##.target Js_of_ocaml.Dom_html.CoerceTo.textarea
                     |> Js.Opt.to_option
                   in
                   Option.map target ~f:(fun target ->
                     Controller.on_tab controller target)
                   |> ignore;
                   Effect.Prevent_default)
                 else Ui_effect.return ())
             ; Attr.on_input (fun _ text ->
                 Ui_effect.Many
                   [ inject (Action.Update_input text)
                   ; Ui_effect.return @@ handle_resize "code_input"
                   ])
             ])
        []
    and res_node =
      let%map compiler_res = m >>| Model.result in
      Node.div
        ~attr:(Attr.many_without_merge [ Attr.style (Css_gen.color (`Name "green")) ])
        [ Node.pre [ Node.text compiler_res ] ]
    and error_node =
      let%map error = m >>| Model.error in
      Node.div
        ~attr:(Attr.many_without_merge [ Attr.style (Css_gen.color (`Name "red")) ])
        [ Node.pre [ Node.text error ] ]
    and configs =
      let%map settings = m >>| Model.controller >>| Controller.settings in
      build_settings settings inject
    in
    Node.body
      [ description_node
      ; problem_node
      ; code_input
      ; res_node
      ; error_node
      ; next_button
      ; configs
      ]
  ;;

  let on_display ~old_model model _ ~schedule_action:_ =
    if not (Model.cutoff old_model model) then handle_resize "code_input"
  ;;

  let create model ~old_model ~inject =
    let open Incr.Let_syntax in
    let%map apply_action =
      let%map model = model in
      apply_action model
    and view = view model ~inject
    and model = model
    and on_display =
      let%map old_model = old_model
      and model = model in
      on_display ~old_model model
    in
    Component.create ~apply_action ~on_display model view
  ;;

  let initial_model = Model.init ()
end
