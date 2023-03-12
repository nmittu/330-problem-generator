open! Core
open Incr_dom

module Js = Js_of_ocaml.Js

module Model = struct
  type t =
    { typ: string
    ; code_input: string
    ; compile_result: string
    ; error: string
    }
  [@@deriving sexp, fields, compare]

  let init () =
    { typ = Type_generator.generate ~mode:(`Polymorphic 3) ~typ:(`Func 1) 4 |> Type_generator.to_string
    ; code_input = ""
    ; compile_result = ""
    ; error = ""
    }

  let type_regex = Str.regexp {|[a-zA-Z- ]* : \(.*\) = .*|}
  
  let compile t =
    match Compiler.execute (String.strip t.code_input) with
    | Ok compile_result ->
       if Str.string_match type_regex compile_result 0 then
         let type_ = Str.matched_group 1 compile_result in
         if String.(type_ = t.typ) then
           { t with compile_result=type_; error=""; }
         else { t with compile_result=""; error=Printf.sprintf "Incorrect type: %s" type_ }
       else {t with compile_result=""; error=compile_result }
    | Error error -> {t with compile_result = ""; error }

  let update_input t code_input =
    let model = { t with code_input=String.strip code_input; compile_result = ""; error = ""} in
    if String.is_suffix ~suffix:";;\n" code_input then
      compile model
    else
      model

  let cutoff t1 t2 = compare t1 t2 = 0
end

module Action = struct
  type t =
    | Update_input of string
    | Next
  [@@deriving sexp]
end

module State = struct
  type t = unit
end

let apply_action model action _ ~schedule_action:_ =
  match (action : Action.t) with
  | Update_input s -> Model.update_input model s
  | Next -> Model.init ()
;;

let on_startup ~schedule_action:_ _ = Async_kernel.return ()

let handle_resize id =
  let open Option.Let_syntax in
  let open Js_of_ocaml.Dom_html in
  let opt =
  let%map target = Js.Opt.to_option (CoerceTo.textarea (getElementById id)) in
  begin
    target##.style##.height := Js.string "0";
    target##.style##.height := Js.string((Int.to_string (target##.scrollHeight + 5)) ^ "px")
  end
  in Option.value ~default:() opt

let view (m : Model.t Incr.t) ~inject =
  let open Incr.Let_syntax in
  let open Vdom in
  let next_button =
    Node.button
      ~attr:(Attr.many_without_merge
               [ Attr.on_click (fun _ -> inject Action.Next) ])
      [Node.text "Next Problem"]
  in
  let%map type_node =
    let%map type_text = m >>| Model.typ in
    Node.div [ Node.text type_text ]
  and code_input =
    let%map code = m >>| Model.code_input in
    let css =
      let (@>) = Css_gen.(@>) in
      Css_gen.width (`Percent (Percent.of_percentage 100.))
      @> Css_gen.resize `None
    in
    Node.textarea
      ~attr:
      (Attr.many_without_merge
         [ Attr.id "input"
         ; Attr.type_ "text"
         ; Attr.string_property "value" code
         ; Attr.string_property "id" "code_input"
         ; Attr.string_property "rows" "1"
         ; Attr.style css
         ; Attr.on_input
             (fun _ text ->
               Ui_effect.Many
                 [ inject (Action.Update_input text)
                 ; Ui_effect.return @@ handle_resize "code_input"])
      ])
      []
  and compiler_res_node =
    let%map compiler_res = m >>| Model.compile_result in
    Node.div
      ~attr:
      (Attr.many_without_merge
       [Attr.style (Css_gen.color (`Name "green"))])
      [ Node.text compiler_res]
  and error_node =
    let%map error = m >>| Model.error in
    Node.div
      ~attr:
      (Attr.many_without_merge
         [ Attr.style (Css_gen.color (`Name "red"))])
      [ Node.text error ]
  in
  Node.body [type_node; code_input; compiler_res_node; error_node; next_button]
;;

let on_display ~old_model model _ ~schedule_action: _ =
  if not (Model.cutoff old_model model) then
    handle_resize "code_input"


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
    on_display ~old_model model in
  Component.create ~apply_action ~on_display model view
;;

let initial_model = Model.init ()
