open! Core

type expr =
  | Var
  | List of expr list
  | Cons of expr * expr
  | Tuple of expr list
  | App of expr * expr list
  | Eq of expr * expr
  | Gt of expr * expr
  | Lt of expr * expr
  | Or of expr * expr
  | And of expr * expr
  | Not of expr
  | If of expr * expr * expr

let rec generate depth =
  if depth = 0
  then Var
  else (
    match Random.int 5 with
    | 0 ->
      let len = Random.int 3 in
      List (List.init len ~f:(fun _ -> generate (depth - 1)))
    | 1 -> Cons (generate (depth - 1), generate_list (depth - 1))
    | 2 ->
      let len = Random.int 2 + 1 in
      Tuple (List.init len ~f:(fun _ -> generate (depth - 1)))
    | 3 ->
      let len = Random.int 2 + 1 in
      App (Var, List.init len ~f:(fun _ -> generate (depth - 1)))
    | _ -> If (generate_func (depth - 1), generate (depth - 1), generate (depth - 1)))

and generate_list depth =
  if depth = 0
  then Var
  else (
    match Random.int 4 with
    | 0 -> Var
    | 1 -> Cons (generate (depth - 1), generate_list (depth - 1))
    | 2 ->
      let len = Random.int 3 in
      List (List.init len ~f:(fun _ -> generate (depth - 1)))
    | _ -> If (generate_bool (depth - 1), generate_list 0, generate_list 0))

and generate_func depth =
  if depth = 0
  then Var
  else (
    match Random.int 5 with
    | 0 -> If (generate_bool (depth - 1), generate_func 0, generate_func 0)
    | _ -> Var)

and generate_bool depth =
  if depth = 0
  then Var
  else (
    match Random.int 7 with
    | 0 -> Eq (generate (min (depth - 1) 1), generate (min (depth - 1) 1))
    | 1 -> Gt (generate (min (depth - 1) 1), generate (min (depth - 1) 1))
    | 2 -> Lt (generate (min (depth - 1) 1), generate (min (depth - 1) 1))
    | 3 -> Or (generate_bool 0, generate_bool 0)
    | 4 -> And (generate_bool 0, generate_bool 0)
    | 5 -> Not (generate_bool (depth - 1))
    | _ -> If (generate_bool 0, generate_bool 0, generate_bool 0))
;;

let get_next () =
  let next = ref "a" in
  fun vars ->
    let n = !next in
    let nexts = if Char.(n.[String.length n - 1] = 'z') then n ^ "a" else n in
    let last = nexts.[String.length nexts - 1] in
    let last = last |> Char.to_int |> succ |> Char.of_int_exn |> Char.to_string in
    let nexts = String.sub nexts ~pos:0 ~len:(String.length nexts - 1) ^ last in
    next := nexts;
    vars := n :: !vars;
    n
;;

let to_string e =
  let next = get_next () in
  let vars = ref [] in
  let rec to_string_h = function
    | Var -> next vars
    | List es ->
      let inner = List.map ~f:to_string_h es |> String.concat ~sep:";" in
      Printf.sprintf "[%s]" inner
    | Cons (e1, e2) -> Printf.sprintf "(%s::%s)" (to_string_h e1) (to_string_h e2)
    | Tuple es ->
      let inner = List.map ~f:to_string_h es |> String.concat ~sep:"," in
      Printf.sprintf "(%s)" inner
    | App (e, es) ->
      let args = List.map ~f:to_string_h es |> String.concat ~sep:" " in
      Printf.sprintf "%s %s" (to_string_h e) args
    | Eq (e1, e2) -> Printf.sprintf "(%s = %s)" (to_string_h e1) (to_string_h e2)
    | Gt (e1, e2) -> Printf.sprintf "(%s > %s)" (to_string_h e1) (to_string_h e2)
    | Lt (e1, e2) -> Printf.sprintf "(%s < %s)" (to_string_h e1) (to_string_h e2)
    | Or (e1, e2) -> Printf.sprintf "(%s || %s)" (to_string_h e1) (to_string_h e2)
    | And (e1, e2) -> Printf.sprintf "(%s && %s)" (to_string_h e1) (to_string_h e2)
    | Not e -> Printf.sprintf "(not %s)" (to_string_h e)
    | If (e1, e2, e3) ->
      Printf.sprintf
        "(if %s then %s else %s)"
        (to_string_h e1)
        (to_string_h e2)
        (to_string_h e3)
  in
  let body = to_string_h e in
  let fun_header =
    let vars = if List.is_empty !vars then [ "a" ] else !vars in
    let vars = List.rev vars |> String.concat ~sep:" " in
    Printf.sprintf "fun %s -> " vars
  in
  let source = fun_header ^ body in
  let open Ocamlformat_lib in
  let res =
    Translation_unit.parse_and_format
      Syntax.Expression
      ~input_name:"problem.ml"
      ~source
      Conf.default
  in
  match res with
  | Ok s -> s
  | Error _ -> failwith ("Failed to generate expr: " ^ source)
;;

let gen_string depth = generate depth |> to_string
