open! Core
open Incr_dom

module Js = Js_of_ocaml.Js

module Model = struct
  type t =
    { typ: string
    ; code_input: string
    ; compile_result: string
    ; error: string
    ; mode_config: [ `Non_polymorphic | `Polymorphic ]
    ; poly_count: int
    ; typ_config: [`Norm | `Func ]
    ; func_count: int
    ; params: int
    }
      [@@deriving sexp, fields, compare]

  let mode t =
    if Poly.(`Polymorphic = t.mode_config) then
      `Polymorphic t.poly_count
    else `Non_polymorphic

  let typ_t t =
    if Poly.(`Func = t.typ_config) then
      `Func t.func_count
    else `Norm
  
  let init () =
    { typ = Type_generator.generate ~mode:(`Polymorphic 3) ~typ:(`Func 1) 4 |> Type_generator.to_string
    ; code_input = ""
    ; compile_result = ""
    ; error = ""
    ; mode_config = `Polymorphic
    ; poly_count = 3
    ; typ_config = `Func
    ; func_count = 1
    ; params = 4
    }

  let validate_conf t =
    let poly_count = min t.poly_count t.params |> max 1 in
    let func_count = min t.func_count (t.params - 1) |> max 1 in
    let params = max t.params 2 in
    { t with poly_count; func_count; params }
  
  let next t =
    let t = validate_conf t in
    { t with
      typ = Type_generator.generate ~mode:(mode t) ~typ:(typ_t t) t.params |> Type_generator.to_string
    ; code_input = ""
    ; compile_result = ""
    ; error = ""
    }

  let update_mode t mode =
    let t = match mode with
      | `Polymorphic n -> { t with mode_config=`Polymorphic; poly_count=n }
      | `Non_polymorphic -> { t with mode_config=`Non_polymorphic }
    in next t
  
  let update_typ t typ =
    let t = match typ with
      | `Func n -> { t with typ_config=`Func; func_count=n }
      | `Norm -> { t with typ_config=`Norm }
    in next t
  
  let update_params t params =
    next { t with params }
    
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
    let model = { t with code_input=code_input; compile_result = ""; error = ""} in
    if String.is_suffix ~suffix:";;\n" code_input then
      compile { model with code_input=String.strip code_input }
    else
      model

  let cutoff t1 t2 = compare t1 t2 = 0
end

module Action = struct
  type t =
    | Update_input of string
    | Update_mode of Type_generator.mode
    | Update_typ of Type_generator.typ
    | Update_params of int
    | Next
  [@@deriving sexp]
end

module State = struct
  type t = unit
end

let apply_action model action _ ~schedule_action:_ =
  match (action : Action.t) with
  | Update_input s -> Model.update_input model s
  | Update_mode m -> Model.update_mode model m
  | Update_typ t -> Model.update_typ model t
  | Update_params p -> Model.update_params model p
  | Next -> Model.next model
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
    let module FocusHook = Attr.Single_focus_hook() in
    Node.textarea
      ~attr:
      (Attr.many_without_merge
         [ Attr.id "input"
         ; Attr.type_ "text"
         ; Attr.string_property "value" code
         ; Attr.string_property "id" "code_input"
         ; Attr.string_property "rows" "1"
         ; FocusHook.attr `Read_the_docs__this_hook_is_unpredictable ~after:Ui_effect.Ignore
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
      [ Node.pre [Node.text compiler_res]]
  and error_node =
    let%map error = m >>| Model.error in
    Node.div
      ~attr:
      (Attr.many_without_merge
         [ Attr.style (Css_gen.color (`Name "red"))])
      [ Node.pre [Node.text error] ]
  and configs =
    let%map mode = m >>| Model.mode_config
    and typ = m >>| Model.typ_config
    and params = m >>| Model.params
    and func_count = m >>| Model.func_count
    and poly_count = m >>| Model.poly_count in

    let get_int_val_by_id id =
      let open Option.Let_syntax in
      let open Js_of_ocaml.Dom_html in
      let%map target = Js.Opt.to_option (CoerceTo.input (getElementById id)) in
      target##.value |> Js.to_string |> Int.of_string
    in

    let poly_on_click _ =
      Option.value
        ~default: (Ui_effect.return ())
        (Option.map (get_int_val_by_id "poly_count")
           ~f:(fun n -> inject (Action.Update_mode (`Polymorphic n))))
    in

    let func_on_clic _ =
      Option.value
        ~default: (Ui_effect.return ())
        (Option.map (get_int_val_by_id "func_count")
           ~f:(fun n -> inject (Action.Update_typ (`Func n))))
    in

    let div_css =
      let (@>) = Css_gen.(@>) in
      Css_gen.display `Inline_block
      @> Css_gen.padding_right (`Em 2)
    in

    let mode_conf =
      Node.div
        ~attr:(Attr.many_without_merge [Attr.style div_css])
        [ Node.text "Parameter Type:"
        ; Node.br ()
        ; Node.input
            ~attr:
            (Attr.many_without_merge
               [ Attr.type_ "radio"
               ; Attr.name "mode"
               ; Attr.id "non_poly"
               ; Attr.value "0"
               ; Attr.bool_property "checked" Poly.(mode = `Non_polymorphic)
               ; Attr.on_click (fun _ -> inject (Action.Update_mode `Non_polymorphic))])
            []
        ; Node.label
          ~attr:(Attr.many_without_merge [Attr.for_ "non_poly"])
          [Node.text "Non Polymorphic"]
        ; Node.br ()
        ; Node.input
            ~attr:
            (Attr.many_without_merge
               [ Attr.type_ "radio"
               ; Attr.name "mode"
               ; Attr.id "poly"
               ; Attr.bool_property "checked" Poly.(mode = `Polymorphic)
               ; Attr.on_click poly_on_click])
            []
        ; Node.label
            ~attr:
            (Attr.many_without_merge [Attr.for_ "poly"])
            [ Node.text "Polymorphic (number of vars) "
            ; Node.input
                ~attr:
                (Attr.many_without_merge
                   [ Attr.type_ "number"
                   ; Attr.id "poly_count"
                   ; Attr.min 1.
                   ; Attr.max (Int.to_float params)
                   ; Attr.string_property "value" (Int.to_string poly_count)
                   ; Attr.on_input (fun e _ -> poly_on_click e)
                   ; Attr.on_keydown (fun _ -> Effect.Prevent_default)])
                []]
              
        ]
    in

    let typ_conf =
      Node.div
        ~attr:(Attr.many_without_merge [Attr.style div_css])
        [ Node.text "Higher order function config:"
        ; Node.br ()
        ; Node.input
            ~attr:
            (Attr.many_without_merge
               [ Attr.type_ "radio"
               ; Attr.name "typ"
               ; Attr.id "norm"
               ; Attr.value "0"
               ; Attr.bool_property "checked" (Poly.(typ = `Norm))
               ; Attr.on_click (fun _ -> inject (Action.Update_typ `Norm))
               ])
            []
        ; Node.label
            ~attr: (Attr.many_without_merge [Attr.for_ "norm"])
            [Node.text "Basic"]
        ; Node.br ()
        ; Node.input
            ~attr:
            (Attr.many_without_merge
               [ Attr.type_ "radio"
               ; Attr.name "typ"
               ; Attr.id "func"
               ; Attr.value "1"
               ; Attr.bool_property "checked" (Poly.(typ = `Func))
               ; Attr.on_click func_on_clic
               ])
            []
        ; Node.label
            ~attr: (Attr.many_without_merge [Attr.for_ "func"])
            [ Node.text "HOF (number of functions) "
            ;  Node.input
                 ~attr:
                 (Attr.many_without_merge
                    [ Attr.id "func_count"
                    ; Attr.type_ "number"
                    ; Attr.min 1.
                    ; Attr.max (Int.to_float (params-1))
                    ; Attr.string_property "value" (Int.to_string func_count)
                    ; Attr.on_input (fun e _ -> func_on_clic e)
                    ; Attr.on_keydown (fun _ -> Effect.Prevent_default)
                 ])
                 []
            ]
        ]
                 
    in
    let params_conf =
      Node.span
        ~attr:(Attr.many_without_merge [Attr.style div_css])
        [ Node.text "Number of params "
        ; Node.input
            ~attr:
            (Attr.many_without_merge
               [ Attr.type_ "number"
               ; Attr.min 2.
               ; Attr.string_property "value" (Int.to_string params)
               ; Attr.on_input (fun _ i -> inject (Action.Update_params (Int.of_string i)))
               ; Attr.on_keydown (fun _ -> Effect.Prevent_default)
               ])
            []
        ]

    in Node.div [mode_conf; typ_conf; params_conf]
    
  in
  Node.body
    [type_node; code_input; compiler_res_node; error_node; next_button; configs]
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
