open! Core
open Type

let default_sigma = "abcd"

let rand_char str =
  let i = Random.int (String.length str) in
  str.[i] |> Char.to_string
;;

let rec generate ?(sigma = default_sigma) depth =
  if depth = 0
  then Var (rand_char sigma)
  else (
    match Random.int 2 with
    | 0 -> Lam (rand_char sigma, generate ~sigma (depth - 1))
    | _ -> App (generate ~sigma (depth - 1), generate ~sigma (depth - 1)))
;;

let rec alpha_step v e1 e2 =
  match e1 with
  | Lam (v', _) when String.(v = v') -> e1
  | Lam (v', e) -> Lam (v', alpha_step v e e2)
  | App (e, e') -> App (alpha_step v e e2, alpha_step v e' e2)
  | Var v' when String.(v = v') -> e2
  | Var v -> Var v
;;

let rec reduce expr =
  match expr with
  | Lam (v, e) -> Lam (v, reduce e)
  | App (e1, e2) ->
    (match reduce e1 with
     | Lam (v, e) -> reduce (alpha_step v e e2)
     | _ as e1 -> App (e1, reduce e2))
  | Var v -> Var v
;;

let normalize expr =
  let nextr = ref 0 in
  let next () =
    incr nextr;
    Printf.sprintf "v%d" !nextr
  in
  let rec normalize scopes expr =
    match expr with
    | Lam (v, e) ->
      let v' = next () in
      let scopes = (v, v') :: scopes in
      Lam (v', normalize scopes e)
    | App (e1, e2) -> App (normalize scopes e1, normalize scopes e2)
    | Var v when List.Assoc.mem scopes ~equal:String.( = ) v ->
      Var (List.Assoc.find_exn scopes ~equal:String.( = ) v)
    | Var v -> Var v
  in
  normalize [] expr
;;
