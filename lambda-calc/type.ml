open! Core

type expr =
  | Lam of string * expr
  | App of expr * expr
  | Var of string
[@@deriving sexp, compare]

let rec to_string expr =
  match expr with
  | Lam (v, e) -> Printf.sprintf "(λ%s. %s)" v (to_string e)
  | App (e1, (App (_, _) as e2)) -> Printf.sprintf "%s (%s)" (to_string e1) (to_string e2)
  | App (e1, e2) -> Printf.sprintf "%s %s" (to_string e1) (to_string e2)
  | Var s -> s
;;
