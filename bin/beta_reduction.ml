open! Core
open! Incr_dom
open! Js_of_ocaml
module App = Problems_web.App.Make (Problems_web.Beta_reduction_controller)

let () =
  Start_app.start
    (module App)
    ~bind_to_element_with_id:"app"
    ~initial_model:App.initial_model
;;
