open! Core
open Incr_dom

module Js = Js_of_ocaml.Js

module Model = struct
  type t =
    { typ: string
    ; code_input: string
    ; compile_result: string
    ; error: string
    ; mode_config: Type_generator.mode
    ; typ_config: Type_generator.typ
    ; params: int
    }
  [@@deriving sexp, fields, compare]

  let init ?(mode=`Polymorphic 3) ?(typ=`Func 1) ?(params=4) () =
    { typ = Type_generator.generate ~mode ~typ params |> Type_generator.to_string
    ; code_input = ""
    ; compile_result = ""
    ; error = ""
    ; mode_config = mode
    ; typ_config = typ
    ; params = params
    }

  let update_mode t mode =
    init ~mode ~typ:t.typ_config ~params:t.params ()
  
  let update_typ t typ =
    init ~mode:t.mode_config ~typ ~params:t.params ()
  
  let update_params t params =
    init ~mode:t.mode_config ~typ:t.typ_config ~params ()

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
  | Next ->
     Printf.printf "%s" (model |> Model.sexp_of_t |> Sexp.to_string);
     Model.init ~mode:(Model.mode_config model) ~typ:(Model.typ_config model) ~params:(Model.params model) ()
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
  and configs =
    let%map mode = m >>| Model.mode_config
    and typ = m >>| Model.typ_config
    and _params = m >>| Model.params in

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

    let list_on_clic _ =
      Option.value
        ~default: (Ui_effect.return ())
        (Option.map (get_int_val_by_id "list_count")
           ~f:(fun n -> inject (Action.Update_typ (`List n))))
    in

    let non_poly = match mode with
      | `Non_polymorphic -> true
      | _ -> false in

    let typ = match typ with
      | `Func _ -> `Func
      | `List _ -> `List
      | `Norm -> `Norm
    in
    
    let mode_conf =
      Node.div
        ~attr:(Attr.many_without_merge [Attr.style (Css_gen.display `Inline_block)])
        [ Node.input
            ~attr:
            (Attr.many_without_merge
               [ Attr.type_ "radio"
               ; Attr.name "mode"
               ; Attr.id "non_poly"
               ; Attr.value "0"
               ; Attr.bool_property "checked" non_poly
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
               ; Attr.value "1"
               ; Attr.bool_property "checked" (not non_poly)
               ; Attr.on_click poly_on_click])
            []
        ; Node.label
            ~attr:
            (Attr.many_without_merge [Attr.for_ "poly"])
            [ Node.text "Polymorphic"
            ; Node.input
                ~attr:
                (Attr.many_without_merge
                   [ Attr.type_ "number"
                   ; Attr.id "poly_count"
                   ; Attr.value "4"
                   ; Attr.min 1.0
                   ; Attr.on_input (fun e _ -> poly_on_click e)])
                []]
              
        ]
    in

    let typ_conf =
      Node.div
        ~attr:(Attr.many_without_merge [Attr.style (Css_gen.display `Inline_block)])
        [ Node.input
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
            [ Node.text "HOF"
            ;  Node.input
                 ~attr:
                 (Attr.many_without_merge
                    [ Attr.id "func_count"
                    ; Attr.type_ "number"
                    ; Attr.min 1.
                    ; Attr.value "1"
                    ; Attr.on_input (fun e _ -> func_on_clic e)
                 ])
                 []
            ]
        ; Node.br ()
          ; Node.input
            ~attr:
            (Attr.many_without_merge
               [ Attr.type_ "radio"
               ; Attr.name "typ"
               ; Attr.id "list"
               ; Attr.value "1"
               ; Attr.bool_property "checked" (Poly.(typ = `List))
               ; Attr.on_click list_on_clic
               ])
            []
        ; Node.label
            ~attr: (Attr.many_without_merge [Attr.for_ "list"])
            [ Node.text "Lists"
            ;  Node.input
                 ~attr:
                 (Attr.many_without_merge
                    [ Attr.id "list_count"
                    ; Attr.type_ "number"
                    ; Attr.min 1.
                    ; Attr.value "1"
                    ; Attr.on_input (fun e _ -> list_on_clic e)
                 ])
                 []
            ]
        ]
                 
    in
    let params_conf =
      Node.span
        ~attr:(Attr.many_without_merge [Attr.style (Css_gen.display `Inline_block)])
        [ Node.text "Params"
        ; Node.input
            ~attr:
            (Attr.many_without_merge
               [ Attr.type_ "number"
               ; Attr.min 1.
               ; Attr.value "4"
               ; Attr.on_input (fun _ i -> inject (Action.Update_params (Int.of_string i)))
               ])
            []
        ]

    in Node.div [mode_conf; typ_conf; params_conf]
    
  in
  Node.body [type_node; code_input; compiler_res_node; error_node; next_button; configs]
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
